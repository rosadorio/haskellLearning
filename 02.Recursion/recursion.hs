module Recursion where 

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- length :: Maybe [a] -> Int
-- it is possible to create another rout for inputs not matching [a]



-- quicksort function: Order list To explore on next session
--quicksort :: Ord a => [a] -> [a]
--quicksort [] = []
--quicksort (x:xs) =
--    let smallerSorted = quicksort [a | a <- xs, a <= x]
--        biggerSorted = quicksort [a | a <- xs, a > x]
--    in  smallerSorted ++ [x] ++ biggerSorted



-- simple sum of list
sumList :: (Num a) => [a] -> a
sumList [] = 0
sumList [x] = x
sumList (x:xs) = x + sumList xs


-- sum Pairs
sumPair :: (Num a) => [a] -> [a]
sumPair []       = []
sumPair [x]      = [x]
sumPair (x:y:zs) = (x + y) : sumPair zs

-- example
--   sumList [1,2,3,4] 
--   1 + sumList [2,3,4]
--   1 + 2 + sumList [3,4]
--   1 + 2 + 3 + sumList [4]
--   1 + 2 + 3 + 4 


maxValue :: [Int] -> Int
maxValue [] = 0
maxValue [x] = x
maxValue (x:xs)
  | x > maxValue xs  = x
  | otherwise          = maxValue xs


-- Concatenation
-- a : [a]
-- [a] ++ [a]


add2 :: Int -> Int
add2 n = n + 2

-- providing function as input of another function
add2All :: (Int -> Int) -> [Int] -> [Int]
add2All _ []     = []
add2All func (x:xs) =  func x : add2All func xs






