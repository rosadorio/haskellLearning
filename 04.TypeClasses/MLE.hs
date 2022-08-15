module MLE where
import Data.Either

myDiv :: Int -> Int -> Int
myDiv n m = n `div` m

-- data Maybe a :: Nothing | Just a

x :: Maybe Int
x = Just 5

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv n m = Just (n `div`m)


data MyMaybe a = Infinity | NaN | Proper a deriving Show

properDiv :: Int -> Int -> MyMaybe Int
properDiv 0 0 = NaN
properDiv _ 0 = Infinity
properDiv n m = Proper (n `div` m)

-- data Either a b :: Left a | Right b

manDiv :: Int -> Int -> Either String Int
manDiv 0 0 = Left "NaN"
manDiv _ 0 = Left "Infinity"
manDiv n m = Right (n `div`m)

data Circle = Circle Float deriving Show

z = Just (Circle 2.3)

safeDivCircles :: Circle -> Circle -> Maybe Circle 
safeDivCircles _ (Circle 0)          = Nothing
safeDivCircles (Circle x) (Circle y) = Just (Circle (x / y))


data Rectangle = Rect Int Int deriving Show   -- Square is actually a Rectangle
sqr1 = Rect 2 3

flipRectangle :: Rectangle -> Rectangle
flipRectangle (Rect x y) = Rect y x 


data List a = Empty | Cons a (List a)

myList :: List Int
myList = Cons 0 (Cons 1 (Cons 2 Empty)) 
-- 0:1:2:[]


-- create instance to show list in proper way
instance Show a => Show (List a) where
  show xs = show' xs True where
    show' Empty True           = "[]"
    show' (Cons y ys) True     = "[" ++ show y ++ "," ++ show' ys False
    show' (Cons y Empty) False =  show y ++ "]"
    show' (Cons y ys) False    =  show y ++ "," ++ show' ys False



data Btree a = Leaf a | Node (Btree a) (Btree a) deriving Show


myTree :: Btree Int
myTree = Node (Leaf 0) (Node (Node (Leaf 2)(Leaf 3)) (Leaf 1)) 

--some Magin happen and mC+ (my custom addition operator for Btres) exist


-- INSTANCES allow us to define methods of typeclasses

-- CLASSES allow us to declare new typeclasses

-- example an addable class (Scalar, Vector, Matrix) sub class of Num
class Addable a where
   (+++) :: a -> a -> a


newtype Scalar a =  Scalar a  deriving Show
newtype Vector a =  Vector [a] deriving Show
newtype Matrix a =  Matrix [[a]] deriving Show

instance Num a => Addable (Scalar a) where
  Scalar x +++ Scalar y  = Scalar (x + y)

instance Num a => Addable (Vector a) where
  Vector xs +++ Vector ys = Vector (zipWith (+) xs ys)

zipWith' :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipWith' _ _ []  = []
zipWith' _ [] _  = []
zipWith' ope (x:xs) (y:ys)  =  zipWith ope x y : zipWith' ope xs ys  

instance Num a => Addable (Matrix a) where
  Matrix xxs +++ Matrix yys = Matrix (zipWith' (+) xxs yys)



