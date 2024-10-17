{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}

{-
This module defines the logic of the game and the communication with the `Board.RenderState`
-}
module GameState where

import RenderState (
  BoardInfo (..),
  Point,
  RenderMessage (..),
  CellType (..), DeltaBoard
 )
import Data.Sequence (Seq(..))
import System.Random (StdGen, Random (randomR))
import Control.Monad.Trans.State.Strict (State, modify, gets, runState)
import Control.Arrow (Arrow((&&&)))

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

type GameStep a = State GameState a

opositeDirection :: Direction -> Direction
opositeDirection North = South
opositeDirection South = North
opositeDirection East = West
opositeDirection West = East

stepInDirection :: Direction -> Point -> Point
stepInDirection North (y, x) = (y - 1, x)
stepInDirection South (y, x) = (y + 1, x)
stepInDirection East (y, x) = (y, x + 1)
stepInDirection West (y, x) = (y, x - 1)

randomPoint :: BoardInfo -> GameStep Point
randomPoint (BoardInfo h w)  = do
  stdGen <- gets gsStdGen
  let (point, stdGen') = randomR ((1, 1), (h, w)) stdGen
  modify \gs -> gs{gsStdGen = stdGen'}
  return point

inSnakeBody :: Point -> Snake -> Bool
inSnakeBody point (Snake _ body) =
  point `elem` body

nextHead :: BoardInfo -> GameStep Point
nextHead (BoardInfo h w) = do
  (Snake (x, y) _, direction) <- gets $ gsSnake &&& gsDirection
  return case direction of
      North -> if x - 1 <= 0 then (w, y) else (x - 1, y)
      South -> if x + 1  > w then (1, y) else (x + 1, y)
      East  -> if y + 1  > h then (x, 1) else (x, y + 1)
      West  -> if y - 1 <= 0 then (x, h) else (x, y - 1)

newApple :: BoardInfo -> GameStep Point
newApple boardInfo = do
  (snake, apple) <- gets $ gsSnake &&& gsApple
  apple' <- randomPoint boardInfo

  if apple' /= apple && not (apple' `inSnakeBody` snake)
    then modify (\gs -> gs{gsApple = apple'}) >> return apple'
    else newApple boardInfo

step :: BoardInfo -> GameStep [RenderMessage]
step boardInfo = do
  (snake, apple) <- gets $ gsSnake &&& gsApple
  hd' <- nextHead boardInfo
  if| hd' `inSnakeBody` snake -> return [GameOver]
    | hd' == apple -> do delta <- extendSnake hd'
                         apple' <- newApple boardInfo
                         return [ScoreIncrement , RenderBoard ((apple', Apple):delta)]
    | otherwise -> do delta <- displaceSnake hd'
                      return [RenderBoard delta]

move :: BoardInfo -> GameState -> ([RenderMessage], GameState)
move boardInfo = runState (step boardInfo)

extendSnake :: Point -> GameStep DeltaBoard
extendSnake hd' = do
  Snake hd body <- gets gsSnake
  modify \gs -> gs{gsSnake = Snake hd' (hd:<|body)}
  return [(hd, SnakeBody) , (hd', SnakeHead)]


displaceSnake :: Point -> GameStep DeltaBoard
displaceSnake hd' = do
  Snake hd body <- gets gsSnake
  case body of
    body':|>tl -> modify (\gs -> gs{gsSnake = Snake hd' (hd:<|body')}) >> return
                  [ (hd, SnakeBody)
                  , (hd', SnakeHead)
                  , (tl, RenderState.Empty)
                  ]
    Data.Sequence.Empty -> modify (\gs -> gs{gsSnake = Snake hd' Data.Sequence.Empty}) >> return
                           [(hd, RenderState.Empty) , (hd', SnakeHead)]

