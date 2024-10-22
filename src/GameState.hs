{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- This module defines the logic of the game and the communication with the `RenderState` -}
module GameState where

import RenderState (BoardInfo (..), Point, RenderMessage (..), CellType (..), DeltaBoard, HasBoardInfo (getBoardInfo))
import Data.Sequence (Seq(..))
import System.Random (StdGen, Random (randomR))
import Control.Arrow (Arrow((&&&)))
import Control.Monad.State.Strict (StateT)
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Control.Monad.State (MonadState, gets, modify)
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

class HasGameState s where
  getGameState :: s -> GameState
  setGameState :: s -> GameState -> s
  modifyGameState :: (GameState -> GameState) -> s -> s

class Monad m => MonadSnake m where
  updateGameState :: Event -> m [RenderMessage]
  updateRenderState :: [RenderMessage] -> m ()

opositeDirection :: Direction -> Direction
opositeDirection North = South
opositeDirection South = North
opositeDirection East = West
opositeDirection West = East

randomPoint :: (MonadReader r m, HasBoardInfo r, MonadState s m, HasGameState s) => m Point
randomPoint = do
  BoardInfo h w <- asks getBoardInfo
  stdGen <- gets $ gsStdGen . getGameState
  let (point, stdGen') = randomR ((1, 1), (h, w)) stdGen
  modify $ modifyGameState \s -> s{gsStdGen = stdGen'}
  return point

inSnakeBody :: Point -> Snake -> Bool
inSnakeBody point = elem point . snakeBody

nextHead :: (MonadReader r m, HasBoardInfo r, MonadState s m, HasGameState s) => m Point
nextHead = do
  BoardInfo h w <- asks getBoardInfo
  (Snake (x, y) _, direction) <- gets $ (gsSnake &&& gsDirection) . getGameState
  return case direction of
      North -> if x - 1 < 1 then (w, y) else (x - 1, y)
      South -> if x + 1 > w then (1, y) else (x + 1, y)
      East -> if y + 1 > h then (x, 1) else (x, y + 1)
      West -> if y - 1 < 1 then (x, h) else (x, y - 1)

newApple :: (MonadState s m, HasGameState s, MonadReader r m, HasBoardInfo r) => m Point
newApple = do
  (snake, apple) <- gets $ (gsSnake &&& gsApple) . getGameState
  apple' <- randomPoint

  if apple' /= apple && not (apple' `inSnakeBody` snake)
    then modify (modifyGameState \s -> s{gsApple = apple'}) >> return apple'
    else newApple

step :: (MonadState s m, HasGameState s, MonadReader r m, HasBoardInfo r) => m [RenderMessage]
step = do
  (snake, apple) <- gets $ (gsSnake &&& gsApple) . getGameState
  hd' <- nextHead
  if| hd' `inSnakeBody` snake -> return [GameOver]
    | hd' == apple -> do delta <- extendSnake hd'
                         apple' <- newApple
                         return [ScoreIncrement , RenderBoard ((apple', Apple):delta)]
    | otherwise -> do delta <- displaceSnake hd'
                      return [RenderBoard delta]

move :: (MonadReader r m, HasBoardInfo r, MonadState s m, HasGameState s) =>
        Event -> m [RenderMessage]
move Tick = step
move (UserEvent direction) = do
  setDirection direction
  step

setDirection :: (MonadState s m, HasGameState s) => Direction -> m ()
setDirection direction' = do
  direction <- gets $ gsDirection . getGameState
  unless (direction' == opositeDirection direction) $
    modify $ modifyGameState \s -> s{gsDirection = direction'}

extendSnake :: (MonadState s m, HasGameState s) => Point -> m DeltaBoard
extendSnake hd' = do
  Snake hd body <- gets $ gsSnake . getGameState
  modify $ modifyGameState \s -> s{gsSnake = Snake hd' (hd:<|body)}
  return [(hd, SnakeBody) , (hd', SnakeHead)]

displaceSnake :: (MonadState s m, HasGameState s) => Point -> m DeltaBoard
displaceSnake hd' = do
  Snake hd body <- gets $ gsSnake . getGameState
  case body of
    body':|>tl -> modify (modifyGameState \s -> s{gsSnake = Snake hd' (hd:<|body')}) >> return
                  [ (hd, SnakeBody)
                  , (hd', SnakeHead)
                  , (tl, RenderState.Empty)
                  ]
    Data.Sequence.Empty -> do modify $ modifyGameState \s -> s{gsSnake = Snake hd' Data.Sequence.Empty}
                              return [(hd, RenderState.Empty) , (hd', SnakeHead)]

