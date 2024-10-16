module Initialization(initGame) where

import qualified RenderState as Render
import qualified GameState as Game
import EventQueue (EventQueue (EventQueue))
import Data.Sequence
import System.Random (getStdGen, randomRIO)
import Control.Concurrent.BoundedChan (newBoundedChan)
import Control.Concurrent (newMVar)

randomPointWithin :: Int -> Int -> IO Render.Point
randomPointWithin h w = (,) <$> randomRIO (1, h) <*> randomRIO (1, w)

randomSnakeHeadAndApplePosWithin :: Int -> Int -> IO (Render.Point, Render.Point)
randomSnakeHeadAndApplePosWithin h w = do
  (snakeHeadPos, applePos) <- (,) <$> randomPointWithin h w <*> randomPointWithin h w
  if snakeHeadPos == applePos
    then randomSnakeHeadAndApplePosWithin h w
    else return (snakeHeadPos, applePos)

initGame :: Int -> Int -> Int -> IO (Render.BoardInfo, Game.GameState, Render.RenderState, EventQueue)
initGame h w initialspeed = do
  (snakeHeadPos, applePos) <- randomSnakeHeadAndApplePosWithin h w
  sg <- getStdGen
  newUserEventQueue <- newBoundedChan 3
  newSpeed <- newMVar initialspeed
  let boardInfo = Render.BoardInfo h w
      gameState = Game.GameState (Game.Snake snakeHeadPos Empty) applePos Game.North sg
      renderState = Render.buildInitialBoard boardInfo snakeHeadPos applePos
      eventQueue = EventQueue newUserEventQueue newSpeed initialspeed
  return (boardInfo, gameState, renderState, eventQueue)
