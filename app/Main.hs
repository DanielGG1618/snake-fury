{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent (forkIO)
import EventQueue (writeUserInput)
import Initialization (initGame)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBinaryMode, hSetBuffering, hSetEcho, stdin, stdout)
import App

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hSetBuffering stdout NoBuffering
  hSetBinaryMode stdout True

  [h, w, fps] <- map read <$> getArgs
  let timeSpeed = 1_000_000 `div` fps
  (boardInfo, gameState, renderState, eventQueue) <- initGame h w timeSpeed

  _ <- forkIO $ writeUserInput eventQueue
  let initialState = AppState gameState renderState
  run boardInfo eventQueue initialState

