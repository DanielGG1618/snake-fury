{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App where

import Control.Concurrent (threadDelay)
import EventQueue (EventQueue, setSpeedOnScore, getEventQueue, HasEventQueue, MonadQueue (..), readEvent)
import GameState (HasGameState(..), GameState, move, MonadSnake (..))
import RenderState
    ( BoardInfo,
      HasRenderState(..),
      HasBoardInfo(..),
      RenderState(rsGameOver),
      updateMessages )
import qualified RenderState (render)
import Control.Monad (unless)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Control.Monad.State (MonadState, StateT, gets, evalStateT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (asks)

data AppState = AppState {
  appGameState :: GameState,
  appRenderState :: RenderState
}

data Env = Env BoardInfo EventQueue

instance HasBoardInfo Env where
  getBoardInfo (Env boardInfo _) = boardInfo

instance HasEventQueue Env where
  getEventQueue (Env _ eventQueue) = eventQueue

instance MonadIO m => MonadQueue (App m) where
  pullEvent = asks getEventQueue >>= liftIO . readEvent

newtype App m a = App {
  runApp :: ReaderT Env (StateT AppState m) a
} deriving (Functor , Applicative, Monad, MonadState AppState, MonadReader Env, MonadIO)

instance HasGameState AppState where
  getGameState = appGameState
  setGameState appState gameState = appState{appGameState = gameState}
  modifyGameState modify appState = setGameState appState (modify $ getGameState appState)

instance HasRenderState AppState where
  getRenderState = appRenderState
  setRenderState appState renderState = appState{appRenderState = renderState}
  modifyRenderState modify appState = setRenderState appState (modify $ getRenderState appState)

instance Monad m => MonadSnake (App m) where
  updateGameState = move
  updateRenderState = updateMessages

class MonadRender m where
  render :: m ()

instance (Monad m, MonadIO m) => MonadRender (App m) where
  render = RenderState.render

gameloop :: ( MonadIO m
            , MonadReader r m, HasBoardInfo r, HasEventQueue r
            , MonadState s m, HasGameState s, HasRenderState s
            , MonadQueue m
            , MonadSnake m
            , MonadRender m
            ) => m ()
gameloop = do
  speed <- setSpeedOnScore
  liftIO $ threadDelay speed

  pullEvent >>= updateGameState >>= updateRenderState >> render

  gameIsOver <- gets $ rsGameOver . getRenderState
  unless gameIsOver gameloop

run :: BoardInfo -> EventQueue -> AppState -> IO ()
run boardInfo eventQueue appState =
  runApp gameloop `runReaderT` Env boardInfo eventQueue `evalStateT` appState

