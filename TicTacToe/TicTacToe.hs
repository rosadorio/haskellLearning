module TicTacToe where

-- 1 Initilize Board
-- 2 Player Input
-- 3 Is Valid Input?
-- 4 Change State or Grid 
-- 5 Check Game State -  Winner, Tie or Next Move (go to step 2.)
-- 6 Game over?  


data Player = PlayerX | PlayerO deriving (Show,Eq)

data CellState = Empty | X | O deriving (Show,Eq)

data Cell = Cell {
            xpos :: Int,
            ypos :: Int,
            state :: CellState
          } deriving (Show,Eq)


type Grid = [Cell]

--  Game State
data GameState = Running | Xwins | Owins | Tie deriving Show 


-- Initialize Grid
gridSize :: Int
gridSize = 3

initializeGrid :: Int -> Grid
initializeGrid size = go size size
  where go 0 _ = []
        go x 0 = go (x-1) size
        go x y = go x (y-1) ++ [Cell x y Empty]

-- same with list comprehension -- much simpler and intuitive
-- initialGridState gridsize = [Cell x y EmptyCell | y <- [1..gridsize], x <- [1..gridsize]] 


-- Player Input





-- Valid Move 
validMove :: Cell -> Grid -> Bool
validMove (Cell x y cstate) gs
  | x > gridSize || x < 1            =  False
  | y > gridSize || y < 1            =  False
  | state (findCell x y gs) /= Empty =  False
  | otherwise                        =  True
  where 
    findCell :: Int -> Int -> Grid -> Cell
    findCell x y gs = head (filter (\el -> xpos el == x && ypos el == y) gs)


-- Change State of Grid
move :: Cell -> Grid -> Grid
move _ []  = []
move newCell (c:cs)  
  | posNewCell     = newCell : move newCell cs 
  | otherwise      = c : move newCell cs
  where posNewCell = (a == x) && (b == y)
        x = xpos newCell
        y = ypos newCell
        a = xpos c
        b = ypos c



--     GRID (x,y)        [Cell] Positions
--  1,1 | 1,2 | 1,3  --   0 | 1 | 2  
--  2,1 | 2,2 | 2,3  --   3 | 4 | 5
--  3,1 | 3,2 | 3,3  --   6 | 7 | 8
--                   Haskell counts from 0

-- Possible win combinatio: if any of the following are X or O that player wins
--          -- horizontal, vertical, diagonal
winComb :: [[Int]]
winComb = [[0,1,2],[3,4,5],[6,7,8],
          [0,3,6],[1,4,7],[2,5,8],
          [0,4,8],[2,4,6]] 

-- check game state
updateGameState :: Grid -> GameState
updateGameState grid
  | checkWin winComb grid == X   = Xwins
  | checkWin winComb grid == O   = Owins
  | noMovesLeft grid             = Tie
  | otherwise                    = Running

-- uses as input the array of win combinations 
checkWin :: [[Int]] -> Grid -> CellState
checkWin [] grid      = Empty
checkWin (wc:wcs) grid
  | stateMatch a b && stateMatch b c && (getState a /= Empty) = getState a
  | otherwise         = checkWin wcs grid
  where a = wc !! 0 -- 
        b = wc !! 1 -- 
        c = wc !! 2 --
        stateMatch c1 c2 = getState c1 == getState c2
        getState pos = state (grid !! pos)


noMovesLeft :: Grid -> Bool
noMovesLeft [] = True
noMovesLeft ((Cell x y state):xs)
  | state == Empty    = False
  | otherwise         = noMovesLeft xs



---- FUNCTION TO SHOW BOARD
showBoard :: Grid -> IO ()
showBoard grid = putStr (makeBoard grid gridSize)

makeBoard :: Grid -> Int -> String
makeBoard [] _ = "" 
makeBoard (g:gs) size
  | ypos g == size   = (show $ state g) ++ "  \n" ++ makeBoard gs size
  | otherwise        = (show $ state g )++ "  |  " ++ makeBoard gs size




