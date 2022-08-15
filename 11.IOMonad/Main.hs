module Main where

import Data.Char

addUnder ::  String -> String
addUnder str = "_" ++ str ++ "_"

-- MONAD uses do notation
main = do
  --actions have this indentation
  putStrLn "What is your name?"
  x <- getLine
  putStrLn ("Hello, " ++ addUnder(map toUpper x))
  putStrLn "zzz....."

-- Sequence of instructions you want Out
-- or
-- Sequences of instructions you want In




test = do
  putStrLn "ME AGAIN"