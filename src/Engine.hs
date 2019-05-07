{-|
Module      : Engine
Description : Snakell snake operative
Copyright   : (c) Borja González Seoane, 2019
License     : MIT
Maintainer  : dev@glezseoane.com
Stability   : experimental
Portability : POSIX

This module closures the operative of the snake character
  of the classic snake game. Snake game is video game
  concept where the player maneuvers a line, the snake,
  which grows in length when it eat pieces of apple. The
  snake itself is the primary obstacle and if its head
  crashes with its body, it deads and the game is over.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Engine
  ( apple
  , barriers
  , initGame
  , step
  , turn
  , Game(..)
  , Direction(..)
  , dead
  , score
  , snake
  , height
  , width
  ) where

import Control.Applicative ((<|>))
import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad (guard)
import Control.Monad.Extra (orM)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.List
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq(..), (<|))
import Linear.V2 (V2(..), _x, _y)
import qualified Data.Sequence as S
import System.Random (Random(..), newStdGen, RandomGen)


-- Types

data Game = Game       -- ^ data set that define a concret game state
  { _snake      :: Snake        -- ^ snake as a sequence of points
  , _dir        :: Direction
  , _apple      :: Coord        -- ^ location of the apple
  , _apples     :: Stream Coord -- ^ list of next apples locations
  , _barriers   :: [Coord]      -- ^ location of the map obstacles
  , _score      :: Int
  , _dead       :: Bool         -- ^ dead so game over flag
  , _paused     :: Bool         -- ^ paused flag
  , _locked     :: Bool         -- ^ disallow duplicate turns between time steps
  , _difficulty :: Int          -- ^ more level, more barriers and velocity
  } deriving (Show)

type Coord = V2 Int

type Snake = Seq Coord

data Stream a = a :| Stream a
  deriving (Show)

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

makeLenses ''Game


-- Constants

height, width :: Int
height = 20
width = 20


-- Auxiliary functions

instance Random a => Random (V2 a) where
  randomR (V2 x1 y1, V2 x2 y2) g =
    let (x, g')  = randomR (x1, x2) g
        (y, g'') = randomR (y1, y2) g'
     in (V2 x y, g'')
  random g =
    let (x, g')  = random g
        (y, g'') = random g'
     in (V2 x y, g'')

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")


-- Functions

-- | Step forward in time
step :: Game -> Game
step s = flip execState s . runMaybeT $ do
  -- Make sure the state isn't paused or over
  MaybeT $ guard . not <$> orM [use paused, use dead]
  -- Unlock from last directional turn
  MaybeT . fmap Just $ locked .= False

  crashSnake <|> crashBarrier <|> eatapple <|> MaybeT (Just <$> modify move)

-- | Possibly die if next head position is in snake
crashSnake :: MaybeT (State Game) ()
crashSnake = do
  MaybeT . fmap guard $ elem <$> (nextHead <$> get) <*> (use snake)
  MaybeT . fmap Just $ dead .= True

-- | Possibly die if next head position is a barrier
crashBarrier :: MaybeT (State Game) ()
crashBarrier = do
  MaybeT . fmap guard $ elem <$> (nextHead <$> get) <*> (use barriers)
  MaybeT . fmap Just $ dead .= True

-- | Possibly eat apple if next head position is apple
eatapple :: MaybeT (State Game) ()
eatapple = do
  MaybeT . fmap guard $ (==) <$> (nextHead <$> get) <*> (use apple)
  MaybeT . fmap Just $ do
    modifying score (+ 1)
    get >>= \g -> modifying snake (nextHead g <|)
    nextapple

-- | Set a valid next apple coordinate
nextapple :: State Game ()
nextapple = do
  (f :| fs) <- use apples
  apples .= fs
  elem f <$> use snake >>= \case
    True -> nextapple
    False -> apple .= f

-- | Move snake along
move :: Game -> Game
move g@Game { _snake = (s :|> _) } = g & snake .~ (nextHead g <| s)
move _                              = error "Snake can not be empty!"

-- | Get next head position of the snake
nextHead :: Game -> Coord
nextHead Game { _dir = d, _snake = (a :<| _) }
  | d == North = a & _y %~ (\y -> (y + 1) `mod` height)
  | d == South = a & _y %~ (\y -> (y - 1) `mod` height)
  | d == East  = a & _x %~ (\x -> (x + 1) `mod` width)
  | d == West  = a & _x %~ (\x -> (x - 1) `mod` width)
nextHead _ = error "Snakes can't be empty!"

-- | Turn game direction
turn :: Direction -> Game -> Game
turn d g = if g ^. locked
  then g
  else g & dir %~ turnDir d & paused .~ False & locked .~ True

turnDir :: Direction -> Direction -> Direction
turnDir n c | c `elem` [North, South] && n `elem` [East, West] = n
            | c `elem` [East, West] && n `elem` [North, South] = n
            | otherwise = c

-- | Creates a barriers list. Difficulty ones are in high list positions
createBarriers :: Int -> Int -> [Coord]
createBarriers 0 _ = []
createBarriers _ 0 = []
createBarriers 1 _ = []
createBarriers _ 1 = []
createBarriers hl vl =
  [ V2 ((hl `div` 2) + 3) (vl `div` 2)
  , V2 ((hl `div` 2) + 4) (vl `div` 2)
  , V2 (hl `div` 4) (vl `div` 3)
  , V2 ((hl `div` 3) + 1) (vl - 5)
  , V2 ((hl `div` 3) + 2) (vl - 5)
  , V2 ((hl `div` 3) + 3) (vl - 5)
  , V2 ((hl `div` 3) + 4) (vl - 5)
  ]

-- | Initialize a paused game with random apple and barriers location
initGame :: IO Game
initGame = do
  (f :| fs) <-
    fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
  let diff = 3
  let bs = take diff (createBarriers width height)
  let xm = width `div` 2
      ym = height `div` 2
      g  = Game
        { _snake      = (S.singleton (V2 xm ym))
        , _apple      = f
        , _apples     = fs
        , _barriers   = bs
        , _score      = 0
        , _dir        = North
        , _dead       = False
        , _paused     = True
        , _locked     = False
        , _difficulty = diff
        }
  return $ execState nextapple g
