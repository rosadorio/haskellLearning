import Control.Monad
import Data.Monoid
import System.Environment
import Data.Semigroup
--import Control.Monad.Reader

newtype Transformer s a = Transformer { runTransformer :: s -> (a,s)}


-- monadic value

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- m a   is a monadic value
-- 'a' is a contextual value

-- ex:
-- Just 7        -> Maybe Monadic Value
-- [2,3,4]       -> List Monadic Valur  
-- Just "Hello"  -> Maybe Monadic Value
-- "hello"       -> 
--  \s -> ((),s) -> Monadic Value is   \s -> ((),s) 
--                  Contextal Value is (), the unit 
-- \s -> ("abs", s) Contextual value is "abc"

-- Left "error"  ->
-- Right 7       ->

-- getLine :: IO String () is itself a Monadic Value 


-- inside the do notation it is all the same monad connect with the bind >>= 


safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

safeAddHeads :: Num a => [a] -> [a] -> Maybe a
safeAddHeads xs ys = do
   x <- safeHead xs  -- if any input is empty it returns Nothing
   y <- safeHead ys
   return (x+y)

-- The monad is a way to simplify the operation without havgin to worry about 


-- State s a = State { runState :: s -> (a,s) }
-- s --> (a,s)

-- Reader e a: s -> a
-- the reader function is ask


-- Writer w a: (a,s)
--      Monoid w => (a,w)
-- the writer function is tell


-- import Data.Text



-- Reader when you want to have acess to your data but dont want to change it
{-# LANGUAGE InstanceSigs -} -- allows defining types of instances
-- reader defined as environment 
newtype Environment e a = Environment { runEnvironment :: e -> a }

instance Functor (Environment e) where
  fmap = liftM

instance Applicative (Environment e) where
  pure  = return
  (<*>) = ap

instance Monad (Environment e) where
 -- return :: a -> Environment e a
  return a = Environment $ \e -> a

-- ea     :: Environment {e -> a}
-- k              :: a -> Environment {e -> b}
-- runEnvironment :: Environment { e -> b}
-- result :: Environment e -> b
--  (>>=) :: Environment e a  -> (a -> Environment e b) -> Environment e b
  ea >>= k  = Environment $ \e -> 
     let a = runEnvironment  ea e in runEnvironment (k a) e



 -- monadic value: e -> a
-- contextual value: a 

type Value = Float
type Indent = String
type Table = [(Indent,Value)]
data Expr 
  = Lit Value          -- a = Value
  | Var Indent         -- Var "x" -> get value of x
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Seq [Expr]         -- seq of expressions
  deriving Show

ask :: Environment e e
ask = Environment $ \e -> e

check :: Maybe Value -> String -> Environment Table Value
check ma s =  case ma of
    Just v  -> return v
    Nothing -> error s


evalReader :: Expr -> Environment Table Value
evalReader (Lit v)   = return v
evalReader (Var x)   = do 
  t <- ask
  check (lookup x t) (concat["cant find ",x])
evalReader (Add e f) = do
  v <- evalReader e
  w <- evalReader f
  return (v+w)
evalReader (Sub e f) = do
  v <- evalReader e
  w <- evalReader f
  return (v-w)
evalReader (Mul e f) = do
  v <- evalReader e
  w <- evalReader f
  return (v * w)
evalReader (Div e f) = do
  v <- evalReader e
  w <- evalReader f
  return (v / w)

constants :: Table
constants = [("pi", 3.14)]
e = Mul (Var "pi") (Mul (Lit 7) (Lit 7))

main = do
  --pi : pi_value :  <- getArgs
  --print . runEnvironment (evalReader e) $ [pi, read pi_value] 
  print . runEnvironment (evalReader e) $ constants


-- Eval writer
-- for this to work w must be a monoid
newtype Logger w a = Logger { runLogger :: (a,w) }

instance Functor (Logger w) where
  fmap h la = let (a, w) = runLogger la in Logger (h a, w)

instance Monoid w => Applicative (Logger w) where
  pure  = return
  (<*>) = ap

instance Monoid w => Monad (Logger w) where
-- return :: Monoid w =>  a -> Logger w a
  return a  = Logger $ (a, mempty)

--  (>>=) :: Logger w a -> (a -> Logger w b) -> Logger w b
  la >>= k  = Logger $  let (a,w1) = runLogger la; (b, w2) = runLogger (k a) 
                        in (b, w1 <> w2)  

logg :: Monoid w => w -> Logger w ()
logg w = Logger ((),w) 


example :: Logger String Int
example = do
  let x = 7
  logg "Just declared x = 7 \n"
  let y = 7
  logg "Just declared y = 7 \n"
  let z = x+y
  logg "Just declared z = x + y \n"
  return z

example2 :: Logger (Sum Int)  String
example2 = do
  logg (Sum 12)
  logg (Sum 23)
  return "goodbye"

example3 :: Logger (IO String) String
example3 = do
  logg getLine
  logg getLine
  return "goodbye"

example4 :: Logger (Min Int) String 
example4 = do
  logg 3
  logg 4
  logg 1
  logg 7
  return "goodbye!"