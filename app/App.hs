{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App where

import Control.Concurrent (threadDelay)
import EventQueue (EventQueue, readEvent, setSpeed)
import GameState (move, HasGameState (..), GameState)
import RenderState (BoardInfo, RenderState (rsGameOver, rsScore), render, HasRenderState (..))
import Control.Monad (unless, (>=>), (<=<))
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Control.Monad.State (MonadState, StateT, gets, evalStateT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import Control.Monad.Identity (Identity(Identity))

data AppState = AppState {
  appGameState :: GameState,
  appRenderState :: RenderState
}

newtype App m a = App {
  runApp :: ReaderT BoardInfo (StateT AppState m) a
} deriving (Functor , Applicative, Monad, MonadState AppState, MonadReader BoardInfo, MonadIO)

instance HasGameState AppState where
  getGameState = appGameState
  setGameState appState gameState = appState{appGameState = gameState}

instance HasRenderState AppState where
  getRenderState = appRenderState
  setRenderState appState renderState = appState{appRenderState = renderState}

gameloop :: (MonadIO m, MonadReader BoardInfo m, MonadState s m, HasGameState s, HasRenderState s) =>
            EventQueue -> m ()
gameloop eventQueue = do
  score <- gets $ rsScore . getRenderState
  speed <- liftIO $ setSpeed score eventQueue
  liftIO $ threadDelay speed

  (liftIO . readEvent) eventQueue
    >>= move
    >>= render
  -- eventQueue & liftIO . readEvent >>= move >>= render
  -- pure eventQueue >>= liftIO . readEvent >>= move >>= render
  -- (render <=< move <=< liftIO . readEvent) eventQueue
  -- (liftIO . readEvent >=> move >=> render) eventQueue

  gameIsOver <- gets $ rsGameOver . getRenderState
  unless gameIsOver $ gameloop eventQueue

run :: BoardInfo -> AppState -> EventQueue -> IO ()
run boardInfo appState eventQueue =
  runApp (gameloop eventQueue) `runReaderT` boardInfo `evalStateT` appState
