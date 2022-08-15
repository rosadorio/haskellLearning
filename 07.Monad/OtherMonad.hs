module OtherMonads where
-- Monads are also called applicative functors
 -- Applicative are monoids of Functors


-- MONADS, FUNCTORS, APPLICATIVES are all typeclasses

-- Semigroup -> Monoid
-- Functor -> Applicative -> Monad


-- Monad List:
--  [1..20] >>= (\x -> [x]) 


import Data.Char
shout :: IO String
shout = do
        xs <- getLine
        return (map toUpper xs ++ "!")

shout' :: IO String
shout' = getLine >>= \s -> return(map toUpper s ++ "!")
