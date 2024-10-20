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
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Reader.Class (ask)

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

type GameStep = ReaderT BoardInfo (State GameState)

opositeDirection :: Direction -> Direction
opositeDirection North = South
opositeDirection South = North
opositeDirection East = West
opositeDirection West = East

randomPoint :: GameStep Point
randomPoint = do
  (BoardInfo h w) <- ask
  stdGen <- lift $ gets gsStdGen
  let (point, stdGen') = randomR ((1, 1), (h, w)) stdGen
  lift $ modify \gs -> gs{gsStdGen = stdGen'}
  return point

inSnakeBody :: Point -> Snake -> Bool
inSnakeBody point (Snake _ body) =
  point `elem` body

nextHead :: GameStep Point
nextHead = do
  (BoardInfo h w) <- ask
  (Snake (x, y) _, direction) <- lift $ gets $ gsSnake &&& gsDirection
  return case direction of
      North -> if x - 1 < 1 then (w, y) else (x - 1, y)
      South -> if x + 1 > w then (1, y) else (x + 1, y)
      East -> if y + 1 > h then (x, 1) else (x, y + 1)
      West -> if y - 1 < 1 then (x, h) else (x, y - 1)

newApple :: GameStep Point
newApple = do
  (snake, apple) <- lift $ gets $ gsSnake &&& gsApple
  apple' <- randomPoint

  if apple' /= apple && not (apple' `inSnakeBody` snake)
    then lift $ modify (\gs -> gs{gsApple = apple'}) >> return apple'
    else newApple

step :: GameStep [RenderMessage]
step = do
  (snake, apple) <- lift $ gets $ gsSnake &&& gsApple
  hd' <- nextHead
  if| hd' `inSnakeBody` snake -> return [GameOver]
    | hd' == apple -> do delta <- extendSnake hd'
                         apple' <- newApple
                         return [ScoreIncrement , RenderBoard ((apple', Apple):delta)]
    | otherwise -> do delta <- displaceSnake hd'
                      return [RenderBoard delta]

move :: BoardInfo -> GameState -> ([RenderMessage], GameState)
move = runState . runReaderT step

extendSnake :: Point -> GameStep DeltaBoard
extendSnake hd' = do
  Snake hd body <- lift $ gets gsSnake
  lift $ modify \gs -> gs{gsSnake = Snake hd' (hd:<|body)}
  return [(hd, SnakeBody) , (hd', SnakeHead)]

displaceSnake :: Point -> GameStep DeltaBoard
displaceSnake hd' = do
  Snake hd body <- lift $ gets gsSnake
  case body of
    body':|>tl -> lift $ modify (\gs -> gs{gsSnake = Snake hd' (hd:<|body')}) >> return
                  [ (hd, SnakeBody)
                  , (hd', SnakeHead)
                  , (tl, RenderState.Empty)
                  ]
    Data.Sequence.Empty -> lift $ modify (\gs -> gs{gsSnake = Snake hd' Data.Sequence.Empty}) >> return
                           [(hd, RenderState.Empty) , (hd', SnakeHead)]

