module HighOrderFunc where

-- curried functions
-- Every function in Haskell officially only takes one parameter. All the functions that accepted several parameters so far have been curried functions

-- max 2 3
-- (max 2) 3

mult3 :: (Num a) => a -> (a -> (a -> a))
mult3 x y z = x * y * z


-- let mult2With9 = mult3 9
-- By calling functions with too few parameters, so to speak, we're creating new functions on the fly. 


--What if we wanted to create a function that takes a number and compares it to 100 
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100


-- Infix functions can also be partially applied by using sections. To section an infix function, simply surround it with parentheses and only supply a parameter on one side
dividedByTen :: (Floating a) => a -> a
dividedByTen = (/10)


-- check if is upper letter
isUpperLetter :: Char -> Bool
isUpperLetter = (`elem` ['A'..'Z'])



--  HIGHER ORDER FUNCTIONS

-- Functions can take functions as parameters and also return functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- make zipWith function 
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []  
zipWith' _ _ [] = [] 
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- examples:
-- zipWith' max [7,5,43,1] [1,2,3,145,64]
-- zipWith' (++) ["foo","bar","baz"] ["fighters","hoppers","aldrin"]

-- example with list of lists as input
-- zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]


 -- Flip simply takes a function and returns a function that is like our original function, only the first two arguments are flipped
-- can be defined simply as (a -> b -> c) -> b -> a -> c
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

-- ex: flip' zip [1,2,3,4,5] "hello"  
-- ex: zipWith (flip' div) [2,2..] [10,8,6,4,2]


-- MAPS  ---------------------------
--map is one of those really versatile higher-order functions that can be used in millions of different ways
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- can be achieved by list comprehension



-- FILTERS
--filter is a function that takes a predicate (a predicate is a function that tells whether something is true or not, so in our case, a function that returns a boolean value) 
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
 | p x        = x : filter' p xs
 | otherwise  = filter' p xs


-- same quicksort algorithm than before but with with filter -
--only readability changes
quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
    let smallerSorted = quicksort (filter (<=x) xs)  
        biggerSorted = quicksort (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted  


-- TAKEWHILE
--takeWhile function takes a predicate and a list and then goes from the beginning of the list and returns its elements while the predicate holds true

--takeWhile (<10000) (filter odd (map (^2) [1..]))

-- takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)]


-- Collatz sequences
-- sequence that start in any number and always finish in 1
chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1) 


numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15  

numLongChains2 :: Int
numLongChains2 =  let isLong xs = length xs > 15 in length (filter isLong (map chain [1..100]))


--- LAMBDAS
-- Lambdas are basically anonymous functions that are used because we need some functions only once. Normally, we make a lambda with the sole purpose of passing it to a higher-order function
-- \x -> \y -> x + y
-- \(a,b) -> a + b

numLongChains3 :: Int  
numLongChains3 = length (filter (\xs -> length xs > 15) (map chain [1..100]))  

--  zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]

-- map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]  



-- FOLD - folds the list up
-- you can fold the list from the left of from the right

-- FOLDL example left fold with sum function
sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs  

sum'' :: (Num a) => [a] -> a  
sum'' = foldl (+) 0

-- elem' func using foldl
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc ) False ys


-- FOLDR example right fold 
map'' :: (a -> b) -> [a] -> [b]  
map'' f xs = foldr (\x acc -> f x : acc) [] xs  
-- same with foldl:
-- map' f xs = foldl (\acc x -> acc ++ [f x]) [] xs


-- Folds can be used to implement any function where you traverse a list once, element by element, and then return something based on that. Whenever you want to traverse a list to return something, chances are you want a fold. That's why folds are, along with maps and filters, one of the most useful types of functions in functional programming.


-- foldl1 and foldr1 functions work much like foldl and foldr, only you don't need to provide them with an explicit starting value

-- (notice no starting values are given)
maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  
  
reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) [] 
  
product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  
  
filter'' :: (a -> Bool) -> [a] -> [a]  
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []  
  
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  
  
last' :: [a] -> a  
last' = foldl1 (\_ x -> x) 

-- Another way to picture right and left folds is like this: say we have a right fold and the binary function is f and the starting value is z. If we're right folding over the list [3,4,5,6], we're essentially doing this: f 3 (f 4 (f 5 (f 6 z))). f is called with the last element in the list and the accumulator, that value is given as the accumulator to the next to last value and so on. If we take f to be + and the starting accumulator value to be 0, that's 3 + (4 + (5 + (6 + 0))). Or if we write + as a prefix function, that's (+) 3 ((+) 4 ((+) 5 ((+) 6 0))). Similarly, doing a left fold over that list with g as the binary function and z as the accumulator is the equivalent of g (g (g (g z 3) 4) 5) 6.


-- SCANL SCANR
-- scanl and scanr are like foldl and foldr, only they report all the intermediate accumulator states in the form of a list

-- scanl (+) 0 [3,5,2,1]  
-- >[0,3,8,10,11]  
-- scanr (+) 0 [3,5,2,1]  
-- >[11,8,3,1,0]

-- scanl1 (\acc x -> if x > acc then x else acc) [5,0,2,3,7,8,3,4,5]
-- scanl (flip (:)) [] [3,2,1]

--Scans are used to monitor the progression of a function that can be implemented as a fold


-- Function Application with $ -- $ function has the lowest precedence
-- ex map ($ 3) [(4+), (10*), (^2), sqrt]   

-- sqrt (3 + 4 + 9)
-- sqrt $ 3 + 4 + 9
-- similar

-- sum (filter (> 10) (map (*2) [2..10]))
-- f $ g $ z x
-- sum $ filter (> 10) $ map (*2) [2..10]

-- FUNCTION COMPOSITION
-- Function composition is right-associative, so we can compose many functions at a time. The expression -- f (g (z x)) is equivalent to 
--(f . g . z) x

-- map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]

-- map (negate . abs) [5,-3,-6,7,-3,2,-19,24]

-- map (negate . sum . tail) [[1..5],[3..6],[1..7]]

-- what about functions that take several parameters
-- replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]