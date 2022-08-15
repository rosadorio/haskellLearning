import Control.Concurrent
import Control.Monad
import System.Console.ANSI
import System.IO

data Event = Tick | Key Char deriving Show

postTick :: Chan Event -> IO ()
postTick chan = forever $ do
  writeChan chan Tick
  threadDelay (5 * 10 ^ 5)

postKey :: Chan Event -> IO ()
postKey chan = forever $ do
  hSetEcho stdin False
  char <- getChar
  writeChan chan (Key char)

main = do
  hSetBuffering stdin NoBuffering
  chan <- newChan
  forkIO $ postKey chan
  forkIO $ postTick chan
  forever $ do
    e <- readChan chan
    print e
