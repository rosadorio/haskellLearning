{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Console.ANSI
import System.Random
import Control.Monad.Reader
import Control.Monad.State
import System.IO
import Control.Lens
import Control.Concurrent


noBuffering :: IO ()
noBuffering = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering 

-- State: Game
data Game = Game
  { _snake     :: Snake
  , _food      :: Food 
  , _score     :: Score
  , _direction :: Direction
  , _status   :: Status
  } deriving Show

type Point = (Int,Int)
type Score = Int 
newtype Snake = Snake {getSnake :: [Point]} deriving Show
newtype Food = Food  {getFood :: Point} deriving Show
data Direction = U | D | L | R deriving Show
data Status = On | Over deriving Show


type Size = (Int,Int)
data Config = Config {screenSize :: Size}

mkConfig :: IO Config 
mkConfig = do 
  Just (row,col) <- getTerminalSize
  return $ Config { screenSize = (row,col) }  

mkFood :: IO Food
mkFood = do
  Just (row,col) <- getTerminalSize
  fRow           <- randomRIO (2,row-1)
  fCol           <- randomRIO (2,col-1) 
  return $ Food (fRow, fCol)

mkGame :: IO Game 
mkGame = do
-- getTerminalSize :: IO (Maybe (Int,Int))  -- from ANSI terminal
  foood <- mkFood 
  return $ Game 
   { _snake     = Snake $ zip (repeat 10) [12..18] 
   , _food      = foood
   , _direction = L 
   , _score     = 0
   , _status    = On
   }
makeLenses ''Game

--generate random number
--randomRIO (bounds) 


updateScore :: (MonadState Game m, MonadIO m) => m ()
updateScore = do
  score %= (+1)
  currScore <- use score
--  game <- get
--  put (game {_score = _score game + 1} )
  liftIO $ setCursorPosition 0 35
  liftIO $ putStr $ "Score: "++ show currScore
--  liftIO $ putStr $ "Score: "++ show (_score game)

-- can be done with Lenses functions
-- makeLenses ''Game
-- updateScore = score %= (+1)

-- we need to make the type mahc the monad stack
-- State Game IO () -> ReaderT Config (StateT Game IO) ()
-- State Game () -> State Game IO ()
insertIO :: State s a -> ReaderT r (StateT s IO) a
insertIO sg = lift $ StateT $ \g -> 
  let (a, g') = runState sg g in return (a, g')


-- this can be done with liftIO function 



-- 1. User press key "Up"
-- 2. we get the key
-- 3. we change the direction according to "Up" unless snake is moving Down
isValidDirection :: Direction -> Direction -> Bool 
isValidDirection U D = False
isValidDirection D U = False
isValidDirection L R = False
isValidDirection R L = False
isValidDirection _ _ = True
    
turnSnake :: (MonadState Game m) => Direction -> m ()
turnSnake d1 = do
  d0 <- fmap _direction get
  if isValidDirection d0 d1
  then direction .= d1
--  then do 
--    game <- get
--    put (game {_direction = d1})   
  else return ()
 
moveHead :: Direction ->  Point -> Point
moveHead D (r,c) = (r+1,c)
moveHead L (r,c) = (r,c-1)
moveHead R (r,c) = (r,c+1)
moveHead U (r,c) = (r-1,c)
 
moveSnake :: (MonadIO m, MonadState Game m) => m ()
moveSnake = do
  game <- get
  newFood <- liftIO mkFood
  let oldSnake = getSnake (_snake game)
      oldDirection = game ^. direction
      oldFood = getFood (_food game)
      newHead = moveHead (_direction game) (head oldSnake)
      newSnake = newHead : init oldSnake
     
  if oldFood == newHead then do
    --put (game {_snake = Snake newSnake}) 
    snake .= Snake (newHead : oldSnake)
    food  .= newFood
    score %= (+1)
  else do
    snake .= Snake (newHead : init oldSnake)


--renderGame :: ReaderT Config (StateT Game IO) ()
-- alternatively
renderGame :: (MonadIO m, MonadReader Config m, MonadState Game m) => m ()
renderGame = do 
  conf <- ask  
  game <- get
  let (mrow,mcol)  = screenSize conf
      Food (frow,fcol)  = _food game
      Snake (shead:sbody) = _snake game
  if frow < 1 || fcol < 1 || frow >= mrow || fcol >= mcol 
  then error "Food is out of bounds"
--  else lift .lift $ do
  else liftIO $ do 
     clearScreen
     -- print limits
     forM_ (zip (repeat 1) [0 .. mcol]) (renderPoint '-')
     forM_ (zip (repeat mrow) [0 .. mcol]) (renderPoint '-')
     forM_ (zip [0 .. mrow] (repeat 1)) (renderPoint '|')
     forM_ (zip [0 .. mrow] (repeat mcol)) (renderPoint '|')
      
     
  -- lift is necessary to match the func type
  --    lift. lift $ clearScreen >> setCursorPosition frow fcol >>  putChar '&'
     renderPoint '&' (frow,fcol)
 -- without currying: forM_ sbody (\(row,col) -> renderPoint (row,col) 'x')
     forM_ sbody (renderPoint 'x')
     renderPoint 'o' shead

     setCursorPosition 0 (mcol - 12)
     putStr $ "Score: "++ show (_score game)
     return ()
     
-- mapM_ runs a function for each eelement of a list
-- forM_ does the same with inverter arguments
     
renderPoint :: Char -> (Int,Int) -> IO ()
renderPoint c (row,col) = do 
     setCursorPosition row col >>  putChar c  
     

-------------------------------------------------
-- ReaderT Config (StateT Game IO)
-- ReaderT r m a = ReaderT { runReaderT :: r -> m a}
-- Config -> (Game -> IO (a,Game)) 

-- monad stack used in this game 
-- read the config, edit game state and include IO

play :: ReaderT Config (StateT Game IO) ()
play = forever $ do
  renderGame
  isInputAvailable <- liftIO (hWaitForInput stdin 500)
  if isInputAvailable 
  then do
    c <- liftIO getChar
    case c of 
      'a' -> turnSnake L >> moveSnake
      's' -> turnSnake D >> moveSnake
      'w' -> turnSnake U >> moveSnake
      'd' -> turnSnake R >> moveSnake
  else moveSnake 
  
-- :t play              
-- play :: ReaderT Config (StateT Game IO) ()

-- :t runReaderT play  
-- Config -> StateT Game IO ()

-- :t runReaderT play (undefined :: Config)
-- StateT Game IO ()

-- :t runStateT (runReaderT play (undefined :: Config))
-- Game -> IO ((),Game)

-- :t runStateT (runReaderT play (undefined :: Config)) (undefined :: Game)
-- IO ((),Game) 
-------------------------------------------------


main :: IO ()
main = do
  noBuffering
  hideCursor
  conf <- mkConfig
  game <- mkGame
  runStateT (runReaderT play conf) game
  _ <- getChar
  showCursor
  return () 
