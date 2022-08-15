module MoreRecursion where


-- \x -> x+x

-- MAPS
--example
map2Ints :: (Int -> Int) -> [Int]  -> [Int]
map2Ints _ [] = []
map2Ints f (x:xs) = f x : map2Ints f xs

-- lambda function - anonymous function 
-- (\x -> x+x) 5

-- map2Ints (\x -> x*x) [1..32]

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

factorial' :: Int -> Int
factorial' n
  | n <= 0      = 1
  | otherwise   = n * factorial (n-1)


-- Identity in an operator that applied to any side of the function will not change the result
-- identity matrix



-- recursion with costume data types
data Health = Healthy | Sick deriving Show
data Temperature = C Int | F Int deriving Show
temps :: [Temperature]
temps = [C 20, C 21, C 22, C 12]

areUsick :: [Temperature] -> [Health]
areUsick [] = []
areUsick ((C x):xs) = (if x > 20 then Healthy else Sick) : areUsick xs


-- another example

data Grade = Grade Int Char deriving Show
grades :: [Grade]
grades = [Grade 65 'A', Grade 23 'B', Grade 35 'B', Grade 52 'C', Grade 56 'C' ]

average :: [Grade] -> Int
average [] = 0
average arr@((Grade n l):xs) = (sumGrades arr) `div` (length arr)
  where
    sumGrades :: [Grade] -> Int
    sumGrades [] = 0
    sumGrades ((Grade n l):xs) = n + sumGrades xs




-- How to do the function if we want to output  [Grade] -> [Grade]


--infic notation is done with ``, ex `div`
-- <COsutme List name> @ (<head>:<tail>)
-- (DataConstructor <variable 1> <variable 2>)


-- Function with costum dataTypes
tempConverterCtoF :: Temperature -> Temperature
tempConverterCtoF (C x) = F (x * 9 `div` 5 + 32)
-- tempConverterCtoF (C 43)
tempConverterFtoC :: Temperature -> Temperature
tempConverterFtoC (F x) = C ((x-32)*5/9)
-- tempConverterCtoF (C 43)
