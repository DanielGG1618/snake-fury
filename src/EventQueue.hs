{- |
This module handles the external events of the game. That is: the user inputs and the time.
-}
module EventQueue where

import Control.Concurrent (
  MVar,
  readMVar,
  swapMVar,
 )
import Control.Concurrent.BoundedChan (
  BoundedChan,
  tryReadChan,
  tryWriteChan,
 )
import GameState (Direction (..))
import System.IO (hReady, stdin)

data Event = Tick | UserEvent Direction

data EventQueue = EventQueue {
  eqDirectionsChan :: BoundedChan Direction,
  eqCurrentSpeed :: MVar Int,
  eqInitialSpeed :: Int
}

speedForScore :: Int -> Int -> Int
speedForScore score initialSpeed =
  let level = fromIntegral $ min (score `quot` 10) 5
      speedFactor = 1 - level / 10.0 :: Double
   in floor $ fromIntegral initialSpeed * speedFactor

setSpeed :: Int -> EventQueue -> IO Int
setSpeed score (EventQueue _ m_currentSpeed initialSpeed) = do
  currentSpeed <- readMVar m_currentSpeed
  let newSpeed = speedForScore score initialSpeed
  if newSpeed /= currentSpeed
    then swapMVar m_currentSpeed newSpeed >> return newSpeed
    else return currentSpeed

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
