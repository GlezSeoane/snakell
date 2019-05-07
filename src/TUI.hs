{-|
Module      : TUI
Description : TUI of Snakell snake game
Copyright   : (c) Borja González Seoane, 2019
License     : MIT
Maintainer  : dev@glezseoane.com
Stability   : experimental
Portability : POSIX

This module closures the TUI presentation of the game.
Uses Brick to achieve a pretty style.
-}

{-# LANGUAGE OverloadedStrings #-}

module TUI where

import Brick
  ( App(..)
  , AttrMap
  , BrickEvent(..)
  , EventM
  , Next
  , Widget
  , customMain
  , neverShowCursor
  , continue
  , halt
  , hLimit
  , vLimit
  , vBox
  , hBox
  , padRight
  , padLeft
  , padTop
  , padAll
  , Padding(..)
  , withBorderStyle
  , str
  , strWrapWith
  , attrMap
  , withAttr
  , emptyWidget
  , AttrName
  , on
  , fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Core
  ( hBox
  , padAll
  , padBottom
  , padLeft
  , padLeftRight
  , padRight
  , padTop
  , padTopBottom
  , str
  , vBox
  )
import Control.Concurrent (threadDelay, forkIO)
import Control.Lens ((^.))
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Linear.V2 (V2(..))
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Data.Sequence as S
import qualified Graphics.Vty as V
import Text.Wrap (defaultWrapSettings, preserveIndentation)

import Engine


-- Types

-- | Tic, tac, tic, tac... Game clock
data Tic = Tic

-- | World cells
data Cell = Snake | Apple | Barrier | Empty


-- App definition

app :: App Game Tic ()
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tic
    threadDelay 150000 -- decides how fast your game moves
  g <- initGame
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app g


-- Handling game events

handleEvent :: Game -> BrickEvent () Tic -> EventM () (Next Game)
handleEvent g (AppEvent Tic)                        = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'i') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g


-- Drawing

drawUI :: Game -> [Widget ()]
drawUI g =
  [ hBox [ padLeftRight 2 $
      drawGrid g     
      , vBox [ padTopBottom 2 $
        drawStats g
        , drawInstructions
        ] 
    ]
  ]

drawStats :: Game -> Widget ()
drawStats g = hLimit 11
  $ vBox [ drawScore (g ^. score)
         , padTop (Pad 2) $ drawState (g ^. dead)
         ]

drawScore :: Int -> Widget ()
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Apples")
  $ C.hCenter
  $ padAll 1
  $ withAttr scoreAttr $ str $ show n

drawInstructions :: Widget ()
drawInstructions = strWrapWith defaultWrapSettings
  { preserveIndentation = True } $
    "Use up, down, right, left to start the game and move the snake.\n\n" <>
    "Use 'i' to init again.\n\n" <>
    "Use 'q' to quit Snakell."

drawState :: Bool -> Widget ()
drawState dead =
  if dead
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
     else withAttr playingAttr $ C.hCenter $ str "ON RUN"

drawGrid :: Game -> Widget ()
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Snakell game")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c `elem` g ^. snake     = Snake
      | c == g ^. apple          = Apple
      | c `elem` g ^. barriers  = Barrier
      | otherwise               = Empty

drawCell :: Cell -> Widget ()
drawCell Snake    = withAttr snakeAttr cw
drawCell Apple    = withAttr appleAttr cw
drawCell Barrier  = withAttr barrierAttr cw
drawCell Empty    = withAttr emptyAttr cw

cw :: Widget ()
cw = str "  "


-- Draw style

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (snakeAttr, V.green `on` V.green)
  , (appleAttr, V.red `on` V.red)
  , (barrierAttr, V.white `on` V.white)
  , (scoreAttr, fg V.yellow `V.withStyle` V.bold)
  , (playingAttr, fg V.green `V.withStyle` V.bold)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]

scoreAttr :: AttrName
scoreAttr = "scoreAttr"

playingAttr, gameOverAttr :: AttrName
playingAttr = "playing"
gameOverAttr = "gameOver"

snakeAttr, appleAttr, emptyAttr :: AttrName
snakeAttr = "snakeAttr"
appleAttr = "appleAttr"
barrierAttr = "barrierAttr"
emptyAttr = "emptyAttr"
