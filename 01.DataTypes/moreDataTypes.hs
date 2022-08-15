module DataTypes2 where

--myCompDataType :: Int -> Float -> Char -> [Integer] -> Integer -> Bool
type ComplicatedDataTypeDef = Int -> Float -> Char -> [Integer] -> Integer -> Bool

-- define position
type Position = (Int,Int)
-- define state change / position change
type StateChange = Position -> Position
-- define possible moves
data Move = North | South | East | West

-- define move function
move :: Move -> StateChange
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)



-- test valid position
validPosition :: StateChange -> Bool
validPosition (x,y)
  | x < 0 || y < 0     = False
  | x > 100 || y > 100 = False  
  | otherwise          = True