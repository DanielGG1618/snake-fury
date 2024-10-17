{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent (
  forkIO,
  threadDelay,
 )
import EventQueue (
  Event (Tick, UserEvent),
  EventQueue (eqInitialSpeed),
  readEvent,
  writeUserInput,
 )
import GameState (GameState (gsDirection), move, opositeDirection)
import Initialization (initGame)
import RenderState (BoardInfo, RenderState (rsGameOver), render, updateRenderState)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBinaryMode, hSetBuffering, hSetEcho, stdin, stdout)
import Control.Monad (unless)

cleanConsole :: IO ()
cleanConsole = putStr "\ESC[2J"

gameloop :: BoardInfo -> GameState -> RenderState -> EventQueue -> IO ()
gameloop boardInfo gstate rstate eventQueue = do
  threadDelay $ eqInitialSpeed eventQueue
  event <- readEvent eventQueue
  let (renderMessage, gstate') =
        case event of
          Tick -> move boardInfo gstate
          UserEvent direction ->
            if gsDirection gstate /= opositeDirection direction
              then move boardInfo $ gstate{gsDirection = direction}
              else move boardInfo gstate
  let rstate' = updateRenderState rstate renderMessage
  let gameIsOver = rsGameOver rstate'

  cleanConsole
  putStr $ render boardInfo rstate'
  unless gameIsOver $
    gameloop boardInfo gstate' rstate' eventQueue

main :: IO ()
main = do
  -- enable reading key strokes
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  hSetBuffering stdout NoBuffering
  hSetBinaryMode stdout True

  [h, w, fps] <- map read <$> getArgs
  let timeSpeed = 1_000_000 `div` fps
  (boardInfo, gameState, renderState, eventQueue) <- initGame h w timeSpeed

  -- print $ show gameState

  _ <- forkIO $ writeUserInput eventQueue
  gameloop boardInfo gameState renderState eventQueue

