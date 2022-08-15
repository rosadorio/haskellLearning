module Datatypes where

x :: Int
x = 12

-- Costum data types

--                       deriving is a way of automaticaly declaring instances to TypeClasses * not always the best way to do it 
-- Will be seen in TypeClasses lesson
data Mood = Sad | Happy deriving (Show, Eq)

-- deriving can be a problem when the instances dont match one to one 
-- example Temperture is Celsius and Farehnheight will fail because the conversion is missing for the equality to work
data Temperature = C Float | F Float deriving (Show,Eq)


y = Sad

-- > Fails to complite because doenst know how to compare type Mood

--func :: Mood -> String
--func mood
--  | mood == Sad = "I am sad"
--  | otherwise = "I dont know how I feel"


--- more complex data types can use the data definition recursively 
-- Type constructor = Data constructor
data BTree  = Leaf Int | Node Int BTree BTree deriving Show

myTree :: BTree
myTree = Node 0 (Leaf 2) (Node 2 (Leaf 3) (Node 3 (Leaf 4) (Leaf 5)))


data Student = Student String Int Float deriving Show
aStudent = Student "Bernardo" 13 65 

getName :: Student -> String
getName (Student a b c) = a

-- same with struct notation
data Student' = Student' {name :: String, age :: Int, weight :: Float} deriving Show
anStudent = Student' {name="Bernardo"
                     ,age=31
                     ,weight=74}
-- get specific field from data
nameStudent = name anStudent

-- Synonyms 
type PBHash = String
type CBHash = String

-- data Blockchain =  Genesis String | Block String Blockchain
data Blockchain =  Genesis CBHash | Block CBHash Blockchain deriving Show
genBlock = Genesis "0"
firstBlock = Block "1" genBlock
secondBlock = Block "2" firstBlock



