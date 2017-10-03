module Main where

import Control.Exception (finally)
import Data.IORef
import Graphics.Vty (Key(..), Modifier(..), Picture, Vty)
import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified Graphics.Vty as Vty

data Vim = Vim
  { vimMode :: Mode
  } deriving Show

data Mode
  = InsertMode
  | NormalMode
  deriving (Eq, Show)

main :: IO ()
main = do
  config :: Vty.Config <-
    Vty.standardIOConfig

  vty :: Vty <-
    Vty.mkVty config

  doneRef :: IORef Bool
    <- newIORef False

  (keyAddHandler, fireKey) <- newAddHandler

  let moment :: MomentIO ()
      moment = do
        eKey :: Event (Key, [Modifier]) <-
          fromAddHandler keyAddHandler

        (bPicture :: Behavior Picture, eDone :: Event ()) <-
          vimGolf eKey

        ePicture :: Event (Future Picture) <-
          changes bPicture

        liftIO . Vty.update vty =<< valueB bPicture
        reactimate (writeIORef doneRef True <$ eDone)
        reactimate' (fmap (Vty.update vty) <$> ePicture)

  network :: EventNetwork <-
    compile moment

  actuate network

  let loop :: IO ()
      loop =
        readIORef doneRef >>= \case
          True -> pure ()
          False -> do
            Vty.nextEvent vty >>= \case
              Vty.EvKey key modifier -> fireKey (key, modifier)
              _ -> pure ()
            loop

  loop `finally` Vty.shutdown vty

vimGolf :: Event (Key, [Modifier]) -> MomentIO (Behavior Picture, Event ())
vimGolf eKey = mdo
  let eEsc :: Event ()
      eEsc = () <$ filterE ((== KEsc) . fst) eKey

  bMode :: Behavior Mode <-
    stepper NormalMode (foldr (unionWith const) never
      [ NormalMode <$ whenE bIsInsertMode eEsc
      , InsertMode <$ whenE bIsNormalMode (filterE (== (KChar 'i', [])) eKey)
      ])

  let bIsNormalMode :: Behavior Bool
      bIsNormalMode = (== NormalMode) <$> bMode

  let bIsInsertMode :: Behavior Bool
      bIsInsertMode = (== InsertMode) <$> bMode

  let bVim :: Behavior Vim
      bVim = Vim
        <$> bMode

  let bPicture :: Behavior Picture
      bPicture = Vty.picForImage . Vty.string Vty.defAttr . show <$> bVim

  -- For now, pressing <Esc> while in normal mode exits vim (ha).
  let eDone :: Event ()
      eDone = whenE bIsNormalMode eEsc

  pure (bPicture, eDone)
