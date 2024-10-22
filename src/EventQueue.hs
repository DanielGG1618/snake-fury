{- | This module handles the external events of the game. That is: the user inputs and the time. -}
module EventQueue where

import Control.Concurrent (MVar, readMVar, swapMVar)
import Control.Concurrent.BoundedChan (BoundedChan, tryReadChan, tryWriteChan)
import GameState (Direction (..), Event (..))
import System.IO (hReady, stdin)
import Control.Monad.State (MonadState, gets, MonadIO (liftIO))
import Control.Monad.Reader (MonadReader)
import Control.Monad (unless, void)
import Control.Monad.Reader.Class (asks)
import RenderState (RenderState(rsScore), HasRenderState (getRenderState))

data EventQueue = EventQueue {
  eqDirectionsChan :: BoundedChan Direction,
  eqCurrentSpeed :: MVar Int,
  eqInitialSpeed :: Int
}

class HasEventQueue r where
  getEventQueue :: r -> EventQueue

class Monad m => MonadQueue m where
  pullEvent :: m Event

speedForScore :: Int -> Int -> Int
speedForScore score initialSpeed =
  let level = fromIntegral $ min (score `quot` 10) 5
      speedFactor = 1 - level / 10.0 :: Double
   in floor $ fromIntegral initialSpeed * speedFactor

setSpeedOnScore :: (MonadIO m, MonadReader r m, HasEventQueue r, MonadState s m, HasRenderState s) => m Int
setSpeedOnScore = do
  EventQueue _ m_currentSpeed initialSpeed <- asks getEventQueue
  currentSpeed <- liftIO $ readMVar m_currentSpeed
  score <- gets $ rsScore . getRenderState

  let newSpeed = speedForScore score initialSpeed
  unless (newSpeed == currentSpeed) $
    void $ liftIO (swapMVar m_currentSpeed newSpeed)

  return newSpeed

-- In StackOverflow we trust.
getKey :: IO [Char]
getKey = reverse <$> getKey' ""
 where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char:chars)

writeUserInput :: EventQueue -> IO ()
writeUserInput queue@(EventQueue userChan _ _) = do
  key <- getKey
  case parseUserInput key of
    Just dir -> tryWriteChan userChan dir >> writeUserInput queue
    Nothing -> writeUserInput queue

parseUserInput :: String -> Maybe Direction

parseUserInput "\ESC[A" = Just North
parseUserInput "w" = Just North
parseUserInput "k" = Just North

parseUserInput "\ESC[D" = Just West
parseUserInput "a" = Just West
parseUserInput "h" = Just West

parseUserInput "\ESC[C" = Just East
parseUserInput "d" = Just East
parseUserInput "l" = Just East

parseUserInput "\ESC[B" = Just South
parseUserInput "s" = Just South
parseUserInput "j" = Just South

parseUserInput _ = Nothing

readEvent :: EventQueue -> IO Event
readEvent (EventQueue userChan _ _) = do
  mv <- tryReadChan userChan
  return $ maybe Tick UserEvent mv

