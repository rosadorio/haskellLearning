module CardValidation where

ccValidator :: [Int] -> Bool
ccValidator cc = (mod ((sum' (doubleOdds (reverse' (dropLast cc)))) + (savelastDigit cc)) 10) == 0


-- steps
-- 1. Drop lasp digit
-- 2. Reverse the numbers
-- 3. Multiply Digits in odd positions by 2
-- 4. Subtract 9 to any result higher that 9
-- 5. Add all numbers together
-- 6. Add the check digit to the sum
-- 7. Check if the total sum can be divided by 10

-- 1.
savelastDigit :: [Int] -> Int
savelastDigit [x] = x
savelastDigit (x:xs) = savelastDigit xs

dropLast :: [Int] -> [Int]
dropLast [x] = []
dropLast (x:xs) = x : dropLast xs

-- 2.
reverse' :: [Int] -> [Int]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- 3. 
doubleOdds :: [Int] -> [Int]
doubleOdds [] = []
doubleOdds [x] = [2*x]
doubleOdds (oddd:evenn:xs) = (oddd*2) : (evenn*2): doubleOdds xs 

-- 4.
sub9 :: [Int] -> [Int]
sub9 [] = [] 
sub9 (x:xs)
  | x > 9      = x - 9 : sub9 xs
  | otherwise  = x : sub9 xs

-- 5.  
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- 6.



-- Operation
cc = [6,0,3,3,9,5,2,0,5,6,3,3,5,1,6,9]
cdg = savelastDigit cc
step1 = dropLast cc
step2 = reverse' step1
step3 = doubleOdds step2
step4 = sub9 step3
step5 = sum' step4
step6 = step5 + cdg
step7 = (mod step6 10) == 0
 

{- List Functions
 head
 tail
 init
 last
 take
 takeWhile
 map
 filter
 zip
 !!
 elem
-}



