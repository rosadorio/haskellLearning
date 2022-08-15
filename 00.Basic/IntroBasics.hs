module IntroBasics where


-- data types
x = 10

-- multiple arguments
f :: (Num a) => a -> a -> a
f x y = y*( x + 1)

inteiro :: Integer
inteiro = 12

-- data types
a :: Bool
a = True

b ::  Char
b = 'B'

c :: String
c = "Bernardo"

e :: [Char]
e = ['B','e','r','n','a']

-------- FUNCTIONS ---------------------------

-- factorial function factorial 3 = 3 *2 * 1
factorial :: Integer -> Integer
--base case
factorial 1 = 1 
factorial x = x * factorial(x-1)

f :: [Int]
f = [1..12]

g :: [Int]
g = [1,4..16]


-- Guards, Case. if then else, Pattern matching
factorial' :: Integer -> Integer
factorial' n
  | n < 0        = -1
  | n == 0       = 1
  | otherwise    = n * factorial' ( n-1)

-- case
factorial'' :: Integer -> Integer
factorial'' n =  case (n<=0) of
  True    -> 1
  False   -> n * factorial'' (n-1) 


factorial''' :: Integer -> Integer
factorial''' n = 
  if n <= 0 then
    1
  else
    n*factorial''' (n-1)



-- lists
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- head:tail pattern => (x:xs)
sumEveryTwo :: [Int] -> [Int]
sumEveryTwo []        = []
sumEveryTwo (x:[])    = [x]
sumEveryTwo (x:y:zs)  = x+y : sumEveryTwo zs 


-- Guards and where
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0 






