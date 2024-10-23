{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BlockArguments #-}

module App where

import Control.Concurrent (threadDelay)
import EventQueue (EventQueue, setSpeedOnScore, MonadEventQueueReader(askEventQueue), MonadQueue (..), readEvent)
import GameState (GameState, MonadGameState (getGameState, putGameState), move, MonadSnake (..))
import RenderState
    ( BoardInfo,
      MonadRenderState(..),
      MonadBoardInfoReader(..),
      RenderState(rsGameOver),
      updateMessages )
import qualified RenderState (render)
import Control.Monad (unless)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Control.Monad.State (MonadState, StateT, gets, evalStateT, modify)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (asks)

data AppState = AppState {
  appGameState :: GameState,
  appRenderState :: RenderState
}

data Env = Env {
  envBoardInfo :: BoardInfo,
  envEventQueue :: EventQueue
}

instance Monad m => MonadBoardInfoReader (App m) where
  askBoardInfo :: Monad m => App m BoardInfo
  askBoardInfo = asks envBoardInfo

instance Monad m => MonadEventQueueReader (App m) where
  askEventQueue :: App m EventQueue
  askEventQueue = asks envEventQueue

instance MonadIO m => MonadQueue (App m) where
  pullEvent = askEventQueue >>= liftIO . readEvent

newtype App m a = App {
  runApp :: ReaderT Env (StateT AppState m) a
} deriving (Functor , Applicative, Monad, MonadState AppState, MonadReader Env, MonadIO)

instance Monad m => MonadGameState (App m) where
  getGameState :: Monad m => App m GameState
  getGameState = gets appGameState
  putGameState :: Monad m => GameState -> App m ()
  putGameState gameState = modify \s -> s{appGameState = gameState}

instance Monad m => MonadRenderState (App m) where
  getRenderState :: Monad m => App m RenderState
  getRenderState = gets appRenderState
  putRenderState :: Monad m => RenderState -> App m ()
  putRenderState renderState = modify \s -> s{appRenderState = renderState}

instance Monad m => MonadSnake (App m) where
  updateGameState = move
  updateRenderState = updateMessages

class MonadRender m where
  render :: m ()

instance (Monad m, MonadIO m) => MonadRender (App m) where
  render = RenderState.render

gameloop :: ( MonadIO m
            , MonadBoardInfoReader m
            , MonadEventQueueReader m
            , MonadRenderState m
            , MonadGameState m
            , MonadQueue m
            , MonadSnake m
            , MonadRender m
            ) => m ()
gameloop = do
  speed <- setSpeedOnScore
  liftIO $ threadDelay speed

  pullEvent >>= updateGameState >>= updateRenderState >> render

  gameIsOver <- getsRenderState rsGameOver
  unless gameIsOver gameloop

run :: BoardInfo -> EventQueue -> AppState -> IO ()
run boardInfo eventQueue appState =
  runApp gameloop `runReaderT` Env boardInfo eventQueue `evalStateT` appState

