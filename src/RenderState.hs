{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}

module RenderState where

import Data.Array (Array, array, (//))
import Data.Foldable (foldl')
import Data.ByteString.Builder (Builder, intDec, char7)
import Control.Monad.Reader (ReaderT(ReaderT, runReaderT))
import Control.Monad.Trans.State.Strict (State, modify, runState)
import Control.Monad.Trans.Reader (asks)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.State.Strict (gets)
import Control.Arrow ((&&&))

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

type RenderStep = Control.Monad.Reader.ReaderT BoardInfo (State RenderState)

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

updateRenderState :: [RenderMessage] -> RenderStep ()
updateRenderState [] = return ()
updateRenderState (RenderBoard delta:rms) = do
  board <- lift $ gets rsBoard
  lift $ modify \s -> s{rsBoard = board // delta}
  updateRenderState rms
updateRenderState (GameOver:rms) = do
  lift $ modify \s -> s{rsGameOver = True}
  updateRenderState rms
updateRenderState (ScoreIncrement:rms) = do
  score <- lift $ gets rsScore
  lift $ modify \s -> s{rsScore = score + 1}
  updateRenderState rms

prettyCell :: CellType -> Builder
prettyCell Empty = "_ "
prettyCell SnakeHead = "@ "
prettyCell SnakeBody = "o "
prettyCell Apple = "X "

prettyScore :: Int -> Builder
prettyScore score = "Score: " <> intDec score <> char7 '\n'

render :: [RenderMessage] -> BoardInfo -> RenderState -> (Builder, RenderState)
render messages = runState . runReaderT (renderStep messages)

renderStep :: [RenderMessage] -> RenderStep Builder
renderStep messages = do
  updateRenderState messages

  gameOver <- lift $ gets rsGameOver
  if gameOver then return "GAME OVER" else do
    (board, score) <- lift $ gets $ rsBoard &&& rsScore
    w <- asks width
    return $ fst $ foldl'
      (\(!str, !i) cell ->
        (str <> prettyCell cell <> (if (i + 1) `mod` w == 0 then char7 '\n' else mempty), i + 1))
      (prettyScore score, 0)
      board

