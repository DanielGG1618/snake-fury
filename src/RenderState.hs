{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-|
This module defines the board. A board is an array of CellType elements indexed by a tuple of ints: the height and width.

for example, The following array represents a 3 by 4 board (left top corner is (1,1); right bottom corner is (3,4)) with a snake at
(2, 2) and (2, 3) and an apple at (3,4)

< ((1,1) : Empty), ((1,2) : Empty), ((1,3) : Empty),     ((1,2) : Empty)
, ((2,1) : Empty), ((2,2) : Snake)  ((2,3) : SnakeHead)  ((2,4) : Empty)
, ((3,1) : Empty), ((3,2) : Empty), ((3,3) : Empty),     ((3,4) : Apple) >

Which would look like this:

- - - -
- 0 $ -
- - - X
-}

module RenderState where

import Data.Array (Array, array, (//))
import Data.Foldable (foldl')

type Point = (Int, Int)

data CellType
  = Empty
  | SnakeHead
  | SnakeBody
  | Apple
  deriving (Show, Eq)

data BoardInfo = BoardInfo {
  height :: Int,
  width :: Int
} deriving (Show, Eq)

type Board = Array Point CellType

type DeltaBoard = [(Point, CellType)]

data RenderMessage
  = RenderBoard DeltaBoard
  | GameOver
  deriving Show

data RenderState = RenderState {
  rsBoard :: Board,
  rsGameOver :: Bool
} deriving Show

emptyBoard :: BoardInfo -> Board
emptyBoard (BoardInfo h w) =
  array ((1, 1), (w, h)) [((x, y), Empty) | x <- [1..w], y <- [1..h]]

initialBoard
  :: BoardInfo
  -> Point -- ^ initial point of the snake
  -> Point -- ^ initial Point of the apple
  -> RenderState
initialBoard boardInfo snake apple =
  let board = emptyBoard boardInfo // [(snake, SnakeHead), (apple, Apple)]
  in RenderState board False

updateRenderState :: RenderState -> RenderMessage -> RenderState
updateRenderState (RenderState board _) (RenderBoard delta) = RenderState (board // delta) False
updateRenderState renderState GameOver = renderState{rsGameOver = True}

prettyCell :: CellType -> String
prettyCell Empty = "_ "
prettyCell SnakeHead = "@ "
prettyCell SnakeBody = "o "
prettyCell Apple = "X "

render :: BoardInfo -> RenderState -> String
render _ (RenderState _ True) = "GAME OVER"
render (BoardInfo _ w) (RenderState board _) =
  fst $ foldl' cellFolder ("", 0) board
  where
    cellFolder (!str, !i) cell =
      ( str <> prettyCell cell <> (if (i + 1) `mod` w == 0 then "\n" else "")
      , i + 1
      )

