module Logic where

 import Data 
 import DataTypes

 grading :: Student -> Student
 grading stu = if grade $ grades stu 
                 then stu { certificates = ["Pass"] }
                 else stu

 grade :: [Float] -> Bool
 grade grades 
  | (sum grades) / (fromIntegral $ length grades) >= 70   = True
  | otherwise                               = False


 myfilter :: (Student -> Bool) -> Batches Student -> Batches Student 
 myfilter _ (Closes)      = Closes
 myfilter f (Batch stu b) = if f stu 
                            then Batch stu (myfilter f b) 
                            else myfilter f b


 -- alternative:  ((elem "FOB") . courses)
 filterByCourse :: String -> Student -> Bool
 filterByCourse course stu = elem course (courses stu)
  

 
 -- instance Applicate Batches where
 --   <*>

 
{-
data Batches = Batch Student Batches | Closes deriving Show

data Student = Student { 
                           name         :: String 
                         , grades       :: [Int]
                         , courses      :: [String]
                         , certificates :: [String]
                         } deriving Show

  

*-> Cons
Integer, DaList
(* -> *)
DaList a

-}