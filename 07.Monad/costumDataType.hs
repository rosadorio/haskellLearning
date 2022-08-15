module DataTypes where

type Position = [Int]
data Piece = King | Queen | Rook | Knight | Bishop | Pawn  deriving (Show,Eq)

-------------------Could Be Instances---------------
data CouldBe a = Error ExitList| Is a | Empty deriving Show

ex = Error InvalidMove
ex1 = Is 12
ex2 = Empty

instance Functor CouldBe where
  fmap f (Error a)  = Error a
  fmap f Empty      = Empty
  fmap f (Is a)     = Is (f a)

instance Applicative CouldBe where
  pure              = Is
  Error a <*> _     = Error a
  _ <*>  (Error a)  = Error a
  Empty <*> _       = Empty
  _  <*> Empty      = Empty
  (Is f) <*> (Is a) = Is (f a) 

instance Monad CouldBe where
  return = Is 
  Error a >>= f        = Error a
  Empty    >>= f        = Empty
  (Is a)    >>= f       = f a  
 
  


