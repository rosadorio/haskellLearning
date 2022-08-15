module TypeClasses where

-- From week 2 ----------------------------

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
------------------------------------------


validMovement :: Position -> Bool
validMovement (a,b)
  | a < 0      = False
  | otherwise  = True


data Temp = C Float | F Float deriving (Show)

x = C 100
y = F 212

instance Eq Temp where
  (==) (C x) (C y)  =  x == y
  (==) (F x) (F y)  =  x == y
  (==) (C x) (F y)  =  (1.8*x +32) == y
  (==) (F x) (C y)  =  (1.8*y +32) == x

instance Ord Temp where
  (>=) (C x) (C y)  =  x >= y
  (>=) (F x) (F y)  =  x >= y
  (>=) (C x) (F y)   = (1.8*x +32) >= y
  (>=) (F x) (C y)  =  (1.8*y +32) >= x
  (<=) (C x) (C y)  =  x <= y
  (<=) (F x) (F y)  =  x <= y
  (<=) (C x) (F y)   = (1.8*x +32) <= y
  (<=) (F x) (C y)  =  (1.8*y +32) <= x

instance Num Temp where
  (C x) + (C y)     = C (x + y)
  (C c) + (F f)     = C (c + (f-32)/1.8)
  (F f) + (C c)     = F (f + (1.8*c + 32))
  (F x) + (F y)     = F (x + y)
  abs (C x)         = C (abs x)
  (C x) * (C y)     = C (x * y)
  signum(C x)       = C (signum x)
  fromInteger n     = C (fromInteger n)
  negate (C x)      = C (negate x)


-- include operation for multiplication by scaler   
class ScalarMultOpe a where
  scM :: Float -> a -> a

instance ScalarMultOpe Temp where
  scM n (C x) = C (n * x)
  scM n (F x) = F (n * x)
  


{-
1. Analyse problem

2. Divide and Conquer: Divide in smaller problems if possible

3. Consider Types

4. Consider the process (the evaluation process)

5. Consider the indentitites and basecases

6. Consider the inputs

7. Conde your functions

-}
