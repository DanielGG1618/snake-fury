{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent (
  forkIO,
  threadDelay,
 )
import EventQueue (
  Event (Tick, UserEvent),
  EventQueue,
  readEvent,
  writeUserInput, setSpeed,
 )
import GameState (GameState (gsDirection), move, opositeDirection)
import Initialization (initGame)
import RenderState (BoardInfo, RenderState (rsGameOver, rsScore), render, updateRenderState)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBinaryMode, hSetBuffering, hSetEcho, stdin, stdout)
import Control.Monad (unless)
import Data.ByteString.Builder (hPutBuilder, Builder)

cleanConsole :: IO ()
cleanConsole = putStr "\ESC[2J"

printBuilder :: Builder -> IO ()
printBuilder = hPutBuilder stdout

gameloop :: BoardInfo -> GameState -> RenderState -> EventQueue -> IO ()
gameloop boardInfo gstate rstate eventQueue = do
  speed <- setSpeed (rsScore rstate) eventQueue
  threadDelay speed
  event <- readEvent eventQueue
  let (renderMessages, gstate') =
        case event of
          Tick -> move boardInfo gstate
          UserEvent direction ->
            if gsDirection gstate /= opositeDirection direction
              then move boardInfo $ gstate{gsDirection = direction}
              else move boardInfo gstate
  let rstate' = updateRenderState rstate renderMessages
  let gameIsOver = rsGameOver rstate'

  cleanConsole
  printBuilder $ render boardInfo rstate'
  unless gameIsOver $
    gameloop boardInfo gstate' rstate' eventQueue

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
  gameloop boardInfo gameState renderState eventQueue

