module HighOrder where

--  OUTLINE

-- 1 High Order Functions
-- -- 1.1 Map
-- -- 1.2 Filter
-- -- 1.3 Foldr Foldl  - accumulator patter applied left to right - right to left
-- -- 1.4 Application
-- -- 1.5 Composition

 add :: Int -> Int -> Int
 add x y = x + y

--currying 
-- Int -> (Int -> Int)
-- x -> (y -> x+y)
 add' :: Int -> Int -> Int
 add' x = \y -> x + y

 twice :: (a -> a) -> a -> a
 twice f x = f (f x)


-- step from credit card verification can be done with map
 doubleEach :: [Int] -> [Int]
 doubleEach []       = []
 doubleEach (x:zs) = x*2: doubleEach zs 

-- definition of map
 map' :: (a -> b) -> [a] -> [b]
 map' _ []        = []
 map' f (x:xs)    = f x : map' f xs

-- definition of filter
 filter' :: (a -> Bool) -> [a] -> [a]
 filter' _ []           = []
 filter' f (x:xs)
  | f x                 = x : filter' f xs
  | otherwise           = filter' f xs

 -- sum' :: [Int] -> Int
 -- sum' [] = 0
 -- sum' (x:xs) = x + sum' xs



--fold (also termed reduce, accumulate, aggregate, compress, or inject)
-- input: 1.{function/operator} 2.{value of basecase/identity } 3. {list}
--  it is like a map but that combines all elements
 myfoldr :: (a -> b -> b) -> b -> [a] -> b
 myfoldr _ nil []           = nil 
 myfoldr f nil (x:xs)       = f x (myfoldr f nil xs)


-- works as an accumulator
 myfoldr' :: (a -> b -> b) -> b -> [a] -> b
 myfoldr' f nil = go   -- list is implied as input to go
  where 
   go []          = nil
   go (x : xs)    = f x (go xs)  -- implies input of a list

-- this definition of mylength asks for a list as input
-- it is implicit due to currying 
-- r is the recusive call
 mylength = myfoldr' (\ x r -> 1 + r) 0

 sum' = myfoldr' (+) 0

-- Examples
-- foldr (+) 0 [1..22]
-- foldr (++) [] ["This"," ","is"," ","an"," ","example"]


-- factorial
 factorial :: Integral a => a -> a
 factorial n 
  | n <= 0      = error "no factorial of negative numbers"
  | otherwise   = foldr (\x r -> x * r) 1 [1..n]


-- create map with foldr
 map'' :: (a -> b) -> [a] -> [b]
 map'' f lst = foldr (\x r -> f x : r) [] lst
-- r is the recursive call
-- foldr f base (x:xs) = f x (foldr f nil xs)
--                (\x r -> f x : r) x (foldr f base xs)

-- create filter
 filter'' :: (a -> Bool) -> [a] -> [a]
 filter'' f lst = foldr (\x -> if (f x) then (x:) else id) [] lst

-- id (identity function) does nothing -  move to the recursive call











-----------------------------
 -- LET AND WHERE NOTATION

 aaa x = let y = x + 3
             z = x + 2
         in  y + z
 
 bbb x = y + z
  where
    y = x + 3 
    z = x + 2
