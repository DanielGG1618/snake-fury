{-# LANGUAGE MultiWayIf #-}

{-
This module defines the logic of the game and the communication with the `Board.RenderState`
-}
module GameState where

import RenderState (
  BoardInfo (..),
  Point,
  RenderMessage (..),
  CellType (..)
 )
import Data.Sequence (Seq(..))
import System.Random (StdGen, Random (randomR))

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

opositeDirection :: Direction -> Direction
opositeDirection North = South
opositeDirection South = North
opositeDirection East = West
opositeDirection West = East

stepInDirection :: Direction -> Point -> Point
stepInDirection North (x, y) = (x, y + 1)
stepInDirection South (x, y) = (x, y - 1)
stepInDirection East (x, y) = (x + 1, y)
stepInDirection West (x, y) = (x - 1, y)

randomPoint :: BoardInfo -> StdGen -> (Point, StdGen)
randomPoint (BoardInfo h w) = randomR ((1, 1), (h, w))

inSnakeBody :: Point -> Snake -> Bool
inSnakeBody point (Snake _ body) =
  point `elem` body

nextHead :: BoardInfo -> GameState -> Point
nextHead (BoardInfo h w) (GameState (Snake hd _) _ direction _) =
  let (x, y) = stepInDirection direction hd
  in (x `mod` w, y `mod` h)

-- | Calculates a new random apple, avoiding creating the apple in the same place, or in the snake body
newApple :: BoardInfo -> GameState -> (Point, StdGen)
newApple boardInfo gameState@(GameState (Snake hd body) prevApple _ stdGen) =
  let apple@(point, _) = randomPoint boardInfo stdGen
  in if point `notElem` prevApple:<|hd:<|body
    then apple
    else newApple boardInfo gameState

move :: BoardInfo -> GameState -> (RenderMessage, GameState)
move boardInfo gameState@(GameState snake@(Snake hd body) apple _ _) =
  let hd' = nextHead boardInfo gameState
  in if | hd' `inSnakeBody` snake -> (GameOver, undefined)
        | hd' == apple ->
          let (apple', stdGen') = newApple boardInfo gameState
              body' = hd':<|body
          in (
            RenderBoard [
              (hd', SnakeHead),
              (hd, SnakeBody),
              (apple, RenderState.Empty),
              (apple', Apple)
            ],
            gameState{gsSnake = Snake hd' body', gsApple = apple', gsStdGen = stdGen'}
          )
        | otherwise -> case body of
          xs:|>x -> (
              RenderBoard [
                (hd', SnakeHead),
                (hd, SnakeBody),
                (x, RenderState.Empty)
              ],
              gameState{gsSnake = Snake hd' (hd:<|xs)}
            )
          Data.Sequence.Empty -> (
              RenderBoard [
                (hd', SnakeHead),
                (hd, RenderState.Empty)
              ],
              gameState{gsSnake = Snake hd' Data.Sequence.Empty}
            )

