{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module RenderState where

import Data.Array (Array, array, (//))
import Data.Foldable (foldl', traverse_)
import Data.ByteString.Builder (Builder, intDec, char7, hPutBuilder, Builder)
import Control.Arrow ((&&&))
import Control.Monad.State (StateT, MonadState, gets, modify)
import Control.Monad.Reader (ReaderT, MonadReader, asks)
import Control.Monad.IO.Class (MonadIO (liftIO))
import System.IO (stdout)

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

newtype RenderStep m a = RenderStep {
  runRenderStep :: ReaderT BoardInfo (StateT RenderState m) a
} deriving(Functor, Applicative, Monad, MonadState RenderState, MonadReader BoardInfo)

class HasRenderState s where
  getRenderState :: s -> RenderState
  setRenderState :: s -> RenderState -> s

instance HasRenderState RenderState where
  getRenderState :: RenderState -> RenderState
  getRenderState = id

  setRenderState :: RenderState -> RenderState -> RenderState
  setRenderState _ = id

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

updateRenderState :: (MonadState s m, HasRenderState s) => RenderMessage -> m ()
updateRenderState message = do
  rstate@(RenderState board score _) <- gets getRenderState
  case message of
    RenderBoard delta -> modify \s -> setRenderState s rstate{rsBoard = board // delta}
    GameOver -> modify \s -> setRenderState s rstate{rsGameOver = True}
    ScoreIncrement -> modify \s -> setRenderState s rstate{rsScore = score + 1}

updateMessages :: (MonadState s m, HasRenderState s) => [RenderMessage] -> m ()
updateMessages = traverse_ updateRenderState

prettyCell :: CellType -> Builder
prettyCell Empty = "_ "
prettyCell SnakeHead = "@ "
prettyCell SnakeBody = "o "
prettyCell Apple = "X "

prettyScore :: Int -> Builder
prettyScore score = "Score: " <> intDec score <> char7 '\n'

render :: (MonadReader BoardInfo m, MonadState state m, HasRenderState state, MonadIO m) =>
          [RenderMessage] -> m ()
render messages = do
  builder <- renderStep messages
  liftIO cleanConsole
  liftIO $ printBuilder builder
  where
    cleanConsole = putStr "\ESC[2J"
    printBuilder = hPutBuilder stdout

renderStep :: (MonadReader BoardInfo m, MonadState s m, HasRenderState s) =>
              [RenderMessage] -> m Builder
renderStep messages = do
  updateMessages messages

  gameOver <- gets $ rsGameOver . getRenderState
  if gameOver then return "GAME OVER" else do
    (board, score) <- gets $ (rsBoard &&& rsScore) . getRenderState
    w <- asks width
    return $ fst $ foldl'
      (\(!str, !i) cell ->
        (str <> prettyCell cell <> (if (i + 1) `mod` w == 0 then char7 '\n' else mempty), i + 1))
      (prettyScore score, 0)
      board

