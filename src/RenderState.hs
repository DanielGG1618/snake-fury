{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}

module RenderState where

import Data.Array (Array, array, (//))
import Data.Array.Base (numElements)
import Data.Foldable (foldl')
import Data.ByteString.Builder (Builder, stringUtf8, string7, int32Dec, intDec, char7)
import Data.IntMap.Merge.Lazy (preserveMissing)

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
  | ScoreIncrement
  deriving Show

data RenderState = RenderState {
  rsBoard :: Board,
  rsScore :: Int,
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
  in RenderState board 0 False

updateRenderState :: RenderState -> [RenderMessage] -> RenderState
updateRenderState renderState [] = renderState
updateRenderState (RenderState board score _) (RenderBoard delta:rms) =
  updateRenderState (RenderState (board // delta) score False) rms
updateRenderState renderState (GameOver:rms) =
  updateRenderState renderState{rsGameOver = True} rms
updateRenderState renderState (ScoreIncrement:rms) =
  updateRenderState renderState{rsScore = rsScore renderState + 1} rms

prettyCell :: CellType -> Builder
prettyCell Empty = string7 "_ "
prettyCell SnakeHead = string7 "@ "
prettyCell SnakeBody = string7 "o "
prettyCell Apple = string7 "X "

prettyScore :: Int -> Builder
prettyScore score = string7 "Score: " <> intDec score <> char7 '\n'

render :: BoardInfo -> RenderState -> Builder
render _ (RenderState _ _ True) = string7 "GAME OVER"
render (BoardInfo _ w) (RenderState board score _) =
  if numElements board == 0 then string7 "For some reason board is empty" else
  fst $ foldl' cellFolder (prettyScore score, 0) board
  where
    cellFolder (!str, !i) cell =
      ( str <> prettyCell cell <> (if (i + 1) `mod` w == 0 then char7 '\n' else mempty)
      , i + 1
      )

