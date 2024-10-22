{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- This module defines the logic of the game and the communication with the `RenderState` -}
module GameState where

import RenderState (
  BoardInfo (..),
  Point,
  RenderMessage (..),
  CellType (..), DeltaBoard
 )
import Data.Sequence (Seq(..))
import System.Random (StdGen, Random (randomR))
import Control.Arrow (Arrow((&&&)))
import Control.Monad.State.Strict (StateT)
import Control.Monad.Reader (MonadReader, ask, ReaderT)
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

opositeDirection :: Direction -> Direction
opositeDirection North = South
opositeDirection South = North
opositeDirection East = West
opositeDirection West = East

randomPoint :: (MonadReader BoardInfo m, MonadState s m, HasGameState s) => m Point
randomPoint = do
  (BoardInfo h w) <- ask
  -- stdGen <- gets gsStdGen
  gstate@(GameState _ _ _ stdGen) <- gets getGameState
  let (point, stdGen') = randomR ((1, 1), (h, w)) stdGen
  modify \s -> setGameState s gstate{gsStdGen = stdGen'}
  return point

inSnakeBody :: Point -> Snake -> Bool
inSnakeBody point (Snake _ body) =
  point `elem` body

nextHead :: (MonadReader BoardInfo m, MonadState s m, HasGameState s) => m Point
nextHead = do
  (BoardInfo h w) <- ask
  (Snake (x, y) _, direction) <- gets $ (gsSnake &&& gsDirection) . getGameState
  return case direction of
      North -> if x - 1 < 1 then (w, y) else (x - 1, y)
      South -> if x + 1 > w then (1, y) else (x + 1, y)
      East -> if y + 1 > h then (x, 1) else (x, y + 1)
      West -> if y - 1 < 1 then (x, h) else (x, y - 1)

newApple :: (MonadState s m, HasGameState s, MonadReader BoardInfo m) => m Point
newApple = do
  -- (snake, apple) <- gets $ gsSnake &&& gsApple
  gstate@(GameState snake apple _ _) <- gets getGameState
  apple' <- randomPoint

  if apple' /= apple && not (apple' `inSnakeBody` snake)
    then modify (\s -> setGameState s gstate{gsApple = apple'}) >> return apple'
    else newApple

step :: (MonadState s m, HasGameState s, MonadReader BoardInfo m) => m [RenderMessage]
step = do
  (snake, apple) <- gets $ (gsSnake &&& gsApple) . getGameState
  hd' <- nextHead
  if| hd' `inSnakeBody` snake -> return [GameOver]
    | hd' == apple -> do delta <- extendSnake hd'
                         apple' <- newApple
                         return [ScoreIncrement , RenderBoard ((apple', Apple):delta)]
    | otherwise -> do delta <- displaceSnake hd'
                      return [RenderBoard delta]

move :: (MonadReader BoardInfo m, MonadState s m, HasGameState s) =>
        Event -> m [RenderMessage]
move Tick = step
move (UserEvent direction) = do
  setDirection direction
  step

setDirection :: (MonadState s m, HasGameState s) => Direction -> m ()
setDirection direction' = do
  gstate@(GameState _ _ direction _) <- gets getGameState
  unless (direction' == opositeDirection direction) $
    modify \s -> setGameState s gstate{gsDirection = direction'}

extendSnake :: (MonadState s m, HasGameState s) => Point -> m DeltaBoard
extendSnake hd' = do
  -- Snake hd body <- gets $ gsSnake . getGameState
  gstate@(GameState (Snake hd body) _ _ _) <- gets getGameState
  modify \s -> setGameState s gstate{gsSnake = Snake hd' (hd:<|body)}
  return [(hd, SnakeBody) , (hd', SnakeHead)]

displaceSnake :: (MonadState s m, HasGameState s) => Point -> m DeltaBoard
displaceSnake hd' = do
  -- Snake hd body <- gets gsSnake
  gstate@(GameState (Snake hd body) _ _ _) <- gets getGameState
  case body of
    body':|>tl -> modify (\s -> setGameState s gstate{gsSnake = Snake hd' (hd:<|body')}) >> return
                  [ (hd, SnakeBody)
                  , (hd', SnakeHead)
                  , (tl, RenderState.Empty)
                  ]
    Data.Sequence.Empty -> do modify \s -> setGameState s gstate{gsSnake = Snake hd' Data.Sequence.Empty}
                              return [(hd, RenderState.Empty) , (hd', SnakeHead)]

