{-# LANGUAGE MultiWayIf #-}

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
import Control.Monad.State (execState)

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
stepInDirection North (y, x) = (y - 1, x)
stepInDirection South (y, x) = (y + 1, x)
stepInDirection East (y, x) = (y, x + 1)
stepInDirection West (y, x) = (y, x - 1)

randomPoint :: BoardInfo -> GameState -> (Point, GameState)
randomPoint (BoardInfo h w) gameState@(GameState _ _ _ stdGen) =
  let (point, stdGen') = randomR ((1, 1), (h, w)) stdGen
  in (point, gameState{gsStdGen = stdGen'})

inSnakeBody :: Point -> Snake -> Bool
inSnakeBody point (Snake _ body) =
  point `elem` body

nextHead :: BoardInfo -> GameState -> Point
nextHead (BoardInfo h w) (GameState (Snake hd _) _ direction _) =
  let (x, y) = stepInDirection direction hd
  in ( if | w < x -> 1
          | x < 1 -> w
          | otherwise -> x
     , if | h < y -> 1
          | y < 1 -> h
          | otherwise -> y
     )

-- | Calculates a new random apple, avoiding creating the apple in the same place, or in the snake body
newApple :: BoardInfo -> GameState -> (Point, GameState)
newApple boardInfo gameState@(GameState (Snake hd body) prevApple _ _) =
  let (point, gameState') = randomPoint boardInfo gameState
  in if point `notElem` prevApple:<|hd:<|body
    then (point, gameState'{gsApple = point})
    else newApple boardInfo gameState

move :: BoardInfo -> GameState -> ([RenderMessage], GameState)
move boardInfo gameState@(GameState snake apple _ _) =
  let hd' = nextHead boardInfo gameState
  in if | hd' `inSnakeBody` snake -> ([GameOver], undefined)
        | hd' == apple -> let (delta, gameState') = extendSnake gameState hd'
                              (apple', gameState'') = newApple boardInfo gameState'
                          in ( [ ScoreIncrement
                               , RenderBoard [(apple', Apple)]
                               , RenderBoard delta
                               ]
                             , gameState''
                             )
        | otherwise -> let (delta, gameState') = displaceSnake gameState hd'
                       in ([RenderBoard delta], gameState')

extendSnake :: GameState -> Point -> (DeltaBoard, GameState)
extendSnake gameState@(GameState (Snake prevHd body) _ _ _) nextHd =
  ( [ (nextHd, SnakeHead)
    , (prevHd, SnakeBody)
    ]
  , gameState {gsSnake = Snake nextHd (prevHd:<|body)}
  )

displaceSnake :: GameState -> Point -> (DeltaBoard, GameState)
displaceSnake gameState@(GameState (Snake prevHd (body:|>tl)) _ _ _) nextHd =
  ( [ (nextHd, SnakeHead)
    , (prevHd, SnakeBody)
    , (tl, RenderState.Empty)
    ]
  , gameState {gsSnake = Snake nextHd (prevHd:<|body)}
  )
displaceSnake gameState@(GameState (Snake prevHd Data.Sequence.Empty) _ _ _) nextHd =
  ( [ (nextHd, SnakeHead)
    , (prevHd, RenderState.Empty)
    ]
  , gameState {gsSnake = Snake nextHd Data.Sequence.Empty}
  )
