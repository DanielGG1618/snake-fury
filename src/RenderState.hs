{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

class HasBoardInfo r where
  getBoardInfo :: r -> BoardInfo

instance HasBoardInfo BoardInfo where
  getBoardInfo = id

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
  modifyRenderState :: (RenderState -> RenderState) -> s -> s

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
  (board, score) <- gets $ (rsBoard &&& rsScore) . getRenderState
  case message of
    RenderBoard delta -> modify $ modifyRenderState \s -> s{rsBoard = board // delta}
    GameOver -> modify $ modifyRenderState \s -> s{rsGameOver = True}
    ScoreIncrement -> modify $ modifyRenderState \s -> s{rsScore = score + 1}

updateMessages :: (MonadState s m, HasRenderState s) => [RenderMessage] -> m ()
updateMessages = traverse_ updateRenderState

prettyCell :: CellType -> Builder
prettyCell Empty = "_ "
prettyCell SnakeHead = "@ "
prettyCell SnakeBody = "o "
prettyCell Apple = "X "

prettyScore :: Int -> Builder
prettyScore score = "Score: " <> intDec score <> char7 '\n'

render :: (MonadReader r m, HasBoardInfo r, MonadState state m, HasRenderState state, MonadIO m) =>
          m ()
render = do
  builder <- renderStep
  liftIO cleanConsole
  liftIO $ printBuilder builder
  where
    cleanConsole = putStr "\ESC[2J"
    printBuilder = hPutBuilder stdout

renderStep :: (MonadReader r m, HasBoardInfo r, MonadState s m, HasRenderState s) =>
              m Builder
renderStep = do
  gameIsOver <- gets $ rsGameOver . getRenderState
  if gameIsOver then return "GAME OVER" else do
    (board, score) <- gets $ (rsBoard &&& rsScore) . getRenderState
    w <- asks $ width . getBoardInfo
    return $ fst $ foldl'
      (\(!str, !i) cell ->
        (str <> prettyCell cell <> (if (i + 1) `mod` w == 0 then char7 '\n' else mempty), i + 1))
      (prettyScore score, 0)
      board

