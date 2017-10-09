{-# language RecordWildCards #-}

module Main where

import Control.Exception (finally)
import Data.IORef
import Graphics.Vty (Image, Key(..), Modifier(..), Picture, Vty)
import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified Graphics.Vty as Vty

data Vim = Vim
  { vimMode      :: !Mode
  , vimCursor    :: !Int
  , vimBuffer    :: !String
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

  let eMode :: Event Mode
      eMode =
        foldr (unionWith const) never
          [ NormalMode <$ whenE bIsInsertMode eEsc
          , InsertMode <$
              whenE bIsNormalMode (filterE (== (KChar 'i', [])) eKey)
          ]

  let eNormalMode :: Event ()
      eNormalMode = () <$ filterE (== NormalMode) eMode

  let eInsertMode :: Event ()
      eInsertMode = () <$ filterE (== InsertMode) eMode

  bMode :: Behavior Mode <-
    stepper NormalMode eMode

  let bIsNormalMode :: Behavior Bool
      bIsNormalMode = (== NormalMode) <$> bMode

  let bIsInsertMode :: Behavior Bool
      bIsInsertMode = (== InsertMode) <$> bMode

  eMoveLeft :: Event () <- do
    let eKeyH :: Event ()
        eKeyH = () <$ filterE (== (KChar 'h', [])) eKey

    let eKeyLeft :: Event ()
        eKeyLeft = () <$ filterE (== (KLeft, [])) eKey

    let eNormalMoveLeft :: Event ()
        eNormalMoveLeft = eKeyH <> eKeyLeft

    let eInsertMoveLeft :: Event ()
        eInsertMoveLeft = eKeyLeft

    switchE eNormalMoveLeft
      (foldr (unionWith const) never
        [ eNormalMoveLeft <$ eNormalMode
        , eInsertMoveLeft <$ eInsertMode
        ])

  eMoveRight :: Event () <- do
    let eKeyL :: Event ()
        eKeyL = () <$ filterE (== (KChar 'l', [])) eKey

    let eKeyRight :: Event ()
        eKeyRight = () <$ filterE (== (KRight, [])) eKey

    let eNormalMoveRight :: Event ()
        eNormalMoveRight = eKeyL <> eKeyRight

    let eInsertMoveRight :: Event ()
        eInsertMoveRight = eKeyRight

    switchE eNormalMoveRight
      (foldr (unionWith const) never
        [ eNormalMoveRight <$ eNormalMode
        , eInsertMoveRight <$ eInsertMode
        ])

  bCursor :: Behavior Int <- do
    let inc :: Mode -> String -> Int -> Int
        inc NormalMode buffer = min (length buffer - 1) . (+1)
        inc InsertMode buffer = min (length buffer) . (+1)

        dec :: Int -> Int
        dec = max 0 . subtract 1

    accumB 0 (unions
      [ inc <$> bMode <*> bBuffer <@ eMoveRight
      , inc <$> bMode <*> bBuffer <@ eInsertChar
      , dec <$ eMoveLeft
      , dec <$ whenE bIsInsertMode eNormalMode
      ])

  eInsertChar :: Event Char <- do
    let eChar :: Event Char
        eChar = filterJust (f <$> eKey)
         where
          f :: (Key, [Modifier]) -> Maybe Char
          f (KChar c, []) = Just c
          f _ = Nothing

    switchE never
      (foldr (unionWith const) never
        [ eChar <$ eInsertMode
        , never <$ eNormalMode
        ])

  bBuffer :: Behavior String <- do
    accumB "foobar" (unions
      [ insertAt <$> bCursor <@> eInsertChar
      ])

  let bVim :: Behavior Vim
      bVim = do
        vimMode   <- bMode
        vimCursor <- bCursor
        vimBuffer <- bBuffer
        pure (Vim{..})

  let bPicture :: Behavior Picture
      bPicture = Vty.picForImage . draw <$> bVim
       where
        draw :: Vim -> Image
        draw vim = Vty.vertCat
          [ Vty.string Vty.defAttr (show vim)
          , drawBuffer
          ]
         where
          drawBuffer :: Image
          drawBuffer = Vty.horizCat (map drawChar (zip [0..] (vimBuffer vim)))

          drawChar :: (Int, Char) -> Image
          drawChar (i, c) = Vty.char attr c
           where
            attr =
              if i == vimCursor vim
              then
                Vty.defAttr
                  `Vty.withForeColor` Vty.black
                  `Vty.withBackColor` Vty.white
              else Vty.defAttr

  -- For now, pressing <Esc> while in normal mode exits vim (ha).
  let eDone :: Event ()
      eDone = whenE bIsNormalMode eEsc

  pure (bPicture, eDone)

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x xs = x:xs
insertAt n x (y:ys) = y : insertAt (n-1) x ys

safeTail :: [a] -> [a]
safeTail = \case
  []   -> []
  _:xs -> xs
