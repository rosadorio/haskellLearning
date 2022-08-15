module Monoids where
import Data.Char


-- Monoid is a algebraic structure that contains an associativaty operation and a identity element - Binary associative operation with an identity
{-
class Monoid m where
  -- mempty :: m

  -- mappend :: m -> m -> m

  -- mconcat :: [m] -> m
  -- mconcat = foldr mappend mempty
-}
-- ex: mconcat [[1,2],[3,4]]


-- EXAMPLE

-- Parameters... Custom config elements on a config file.


--Chat bot
--Parameters of the filters for the spam filter
--We want to avoid emoji, kanjis, nothing that is not a letter or number and a few symbols.

-- [ifEmoji,ifKanji, ifLower,ifDigit
filters :: [(Char->Bool)] 
filters = [ifEmoji, ifKanji, ifLower, ifDigit] 

ifEmoji :: Char -> Bool
ifEmoji c = False

ifKanji :: Char -> Bool 
ifKanji c = False

ifLower :: Char -> Bool
ifLower c = isLower c

ifDigit :: Char -> Bool
ifDigit c = isDigit c


isForbidden :: Char -> Bool
isForbidden c = ifEmoji c || ifKanji c || ifLower c  -- any aditional parameter will require code updating
    --  
mForbidden :: Char-> [(Char -> Bool)] -> Bool
mForbidden c filters =  error "to implement"
    --  predicates this is the list of parameters, that can change
    -- filters [ifEmoji,ifKanji, ifLower,ifDigit]


newtype Any = Any {getAny :: Bool} deriving (Show)

instance Semigroup Any where
  (<>) x y        = x

instance Monoid Any where
  -- mempty :: as
  mempty          = Any False 
  -- mappend :: a -> a -> as
  mappend c d = Any $ (getAny c) || (getAny d)


newtype All = All {getAll :: Bool} deriving Show

instance Semigroup All where
  (<>) x y        = x

instance Monoid All where
  -- mempty :: as
  mempty          = All True 
  -- mappend :: a -> a -> as
  mappend c d = All $ (getAll c) && (getAll d)


fnc :: [(Int -> Int)] -> [Int] -> [Int]
fnc xs ys = foldr (\x rec-> map x ys ++ rec) [] xs

fnc' fList nbList = [funct nb | funct <- fList, nb <- nbList]


-- REMEMBER FOLD DEFINITION:

-- if the list is empty, the result is the initial value z; else
-- apply f to the first element and the result of folding the rest

-- foldr f z []     = z 
-- foldr f z (x:xs) = f x (foldr f z xs) 

-- if the list is empty, the result is the initial value; else
-- we recurse immediately, making the new initial value the result
-- of combining the old initial value with the first element.

-- foldl f z []     = z                  
-- foldl f z (x:xs) = foldl f (f z x) xs


-- Implementation of the Maybe Monoid (called Optional)
data Optional a = Nada | Only a deriving (Show,Eq)

instance Semigroup a => Semigroup (Optional a) where
  Nada <> Nada     = Nada
  Nada <> Only a   = Only a
  Only a <> Nada   = Only a
  Only a <> Only b = Only (a <> b)


instance Semigroup a => Monoid (Optional a) where
  mempty   = Nada