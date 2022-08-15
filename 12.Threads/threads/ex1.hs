import Control.Concurrent
import Control.Monad
import System.IO


main = do
  hSetBuffering stdin NoBuffering
  chan <- newChan
  forkIO $ getKey chan
  forever $ do 
    k <- readChan chan
    print k



{-
  forkIO . forever $ do
    putStrLn "Hello, from the other thread"
    threadDelay (10^6)
  forever $ do
  putStrLn "Hello, from the main thread"
  threadDelay (5*10^6)
-}

data Key = Key Char deriving Show
data Event = Tick | Key Char deriving Show

getKey :: Chan Key -> IO ()
getKey chan = do 
  hSetEcho stdout False
  char <- getChar
  writeChan chan (Key char)
  

postTick :: Chan Key -> IO ()
postTick chan = do
  writeChan chan Tick
  threadDelay (10 ^ 5)


