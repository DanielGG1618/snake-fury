{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- This module defines the logic of the game and the communication with the `RenderState` -}
module GameState where

import RenderState (BoardInfo (..), Point, RenderMessage (..), CellType (..), DeltaBoard, MonadBoardInfoReader (..))
import Data.Sequence (Seq(..))
import System.Random (StdGen, Random (randomR))
import Control.Arrow (Arrow((&&&)))
import Control.Monad.State.Strict (StateT)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.State (MonadState)
import Control.Monad (unless)

data Direction = North | South | East | West deriving (Show, Eq)

data Snake = Snake {
  snakeHead :: Point,
  snakeBody :: Seq Point
} deriving (Show, Eq)

data GameState = GameState {
  gsSnake :: Snake,
  gsApple :: Point,
  gsDirection :: Direction,
  gsStdGen :: StdGen
} deriving (Show, Eq)

data Event = Tick | UserEvent Direction

newtype GameStep m a = GameStep {
  runGameStep :: ReaderT BoardInfo (StateT GameState m) a
} deriving (Functor, Applicative, Monad, MonadState GameState, MonadReader BoardInfo)

class Monad m => MonadGameState m where
  getGameState :: m GameState
  putGameState :: GameState -> m ()

  getsGameState :: (GameState -> a) -> m a
  getsGameState f = f <$> getGameState
  modifyGameState :: (GameState -> GameState) -> m ()
  modifyGameState f = getGameState >>= putGameState . f

opositeDirection :: Direction -> Direction
opositeDirection North = South
opositeDirection South = North
opositeDirection East = West
opositeDirection West = East

randomPoint :: (MonadBoardInfoReader m, MonadGameState m) => m Point
randomPoint = do
  BoardInfo h w <- askBoardInfo
  stdGen <- getsGameState  gsStdGen
  let (point, stdGen') = randomR ((1, 1), (h, w)) stdGen
  modifyGameState \s -> s{gsStdGen = stdGen'}
  return point

inSnakeBody :: Point -> Snake -> Bool
inSnakeBody point = elem point . snakeBody

nextHead :: (MonadBoardInfoReader m, MonadGameState m) => m Point
nextHead = do
  BoardInfo h w <- askBoardInfo
  (Snake (x, y) _, direction) <- getsGameState $ gsSnake &&& gsDirection
  return case direction of
      North -> if x - 1 < 1 then (w, y) else (x - 1, y)
      South -> if x + 1 > w then (1, y) else (x + 1, y)
      East -> if y + 1 > h then (x, 1) else (x, y + 1)
      West -> if y - 1 < 1 then (x, h) else (x, y - 1)

newApple :: (MonadGameState m, MonadBoardInfoReader m) => m Point
newApple = do
  (snake, apple) <- getsGameState $ gsSnake &&& gsApple
  apple' <- randomPoint

  if apple' /= apple && not (apple' `inSnakeBody` snake)
    then modifyGameState (\s -> s{gsApple = apple'}) >> return apple'
    else newApple

step :: (MonadGameState m, MonadBoardInfoReader m) => m [RenderMessage]
step = do
  (snake, apple) <- getsGameState $ gsSnake &&& gsApple
  hd' <- nextHead
  if| hd' `inSnakeBody` snake -> return [GameOver]
    | hd' == apple -> do delta <- extendSnake hd'
                         apple' <- newApple
                         return [ScoreIncrement , RenderBoard ((apple', Apple):delta)]
    | otherwise -> do delta <- displaceSnake hd'
                      return [RenderBoard delta]

move :: (MonadBoardInfoReader m, MonadGameState m) =>
        Event -> m [RenderMessage]
move Tick = step
move (UserEvent direction) = do
  setDirection direction
  step

setDirection :: MonadGameState m => Direction -> m ()
setDirection direction' = do
  direction <- getsGameState gsDirection
  unless (direction' == opositeDirection direction) $
    modifyGameState \s -> s{gsDirection = direction'}

extendSnake :: MonadGameState m => Point -> m DeltaBoard
extendSnake hd' = do
  Snake hd body <- getsGameState gsSnake
  modifyGameState \s -> s{gsSnake = Snake hd' (hd:<|body)}
  return [(hd, SnakeBody) , (hd', SnakeHead)]

displaceSnake :: MonadGameState m => Point -> m DeltaBoard
displaceSnake hd' = do
  Snake hd body <- getsGameState gsSnake
  case body of
    body':|>tl -> modifyGameState (\s -> s{gsSnake = Snake hd' (hd:<|body')}) >> return
                  [ (hd, SnakeBody)
                  , (hd', SnakeHead)
                  , (tl, RenderState.Empty)
                  ]
    Data.Sequence.Empty -> do modifyGameState \s -> s{gsSnake = Snake hd' Data.Sequence.Empty}
                              return [(hd, RenderState.Empty) , (hd', SnakeHead)]

