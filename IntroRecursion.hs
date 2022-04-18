module IntroRecursion where

-- case
head' :: [a] -> a  
head' xs = case xs of []    -> error "No head for empty lists!"  
                      (x:_) -> x 

describeList :: [a] -> String  
describeList xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."


-- recursion

--fibonacci sequence
fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- maximum function
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise   = maxTail  
    where maxTail = maximum' xs  

-- replicate list n times
replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x 


-- take first n numbers from list
take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs  

-- reverse the list 
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- repeat x infinite times
repeat' :: a -> [a]  
repeat' x = x:repeat' x  

-- zip takes two lists and zips them together
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys


-- elem takes an element and a list and sees if that element is in the list
elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs 


-- quicksort function: Order list
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

{-
when trying to think of a recursive way to solve a problem, try to think of when a recursive solution doesn't apply and see if you can use that as an edge case, think about identities and think about whether you'll break apart the parameters of the function (for instance, lists are usually broken into a head and a tail via pattern matching) and on which part you'll use the recursive call
-}


-- self exercise ---


-- find if 3D array are proportional
equivalence :: (Integral a) => [a] -> [a] -> Bool
equivalence [a,b,c] [a',b',c'] =  ((div a a' == div b b') && (div a a' == div c c') && (rem a a' == rem b b') && (rem a a' == rem c c')) || ((div a' a == div b' b) && (div a' a == div c' c) && (rem a' a == rem b' b) && (rem a' a == rem c' c)) 


isNotEq3D :: (Integral a) => [a] -> [[a]] -> Bool
isNotEq3D [_,_,_] ([]) = True
isNotEq3D [a,b,c] ([a',b',c']:xs)
    | equivalence [a,b,c] [a',b',c']  = False
    | otherwise                       = isNotEq3D [a,b,c] xs

-- get list right triangles with max length 100
rightTriangles=[ [a,b,c] | c <- [1..200], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2] 

--filter right triangles
filterEqTriangles ::  (Integral p) => [[p]]->[[p]] 
filterEqTriangles [a,b,c] = [a,b,c]
filterEqTriangles ([a,b,c]:xs)
   | isNotEq3D [a,b,c] xs = [a,b,c] : filterEqTriangles xs
   | otherwise = filterEqTriangles xs

-- filtered list of triangles
filteredTriangles =  reverse (filterEqTriangles (reverse rightTriangles))

-- IT WORKS :) --
