import qualified Data.Map as Map
import Control.Monad.State 
-- We also have Monad Reader and Writer


------ STATE MONAD -------------
-----Attempt to hide the plumbing in a monadic value 


-- Map is a tupple like a dictionary or lookup table
-- (index, state) 
-- (code, message) 

type Value = Int
type Indent = String
type Table = [(Indent,Value)]
data Expr 
  = Lit Value          -- a = Value
  | Var Indent         -- Var "x" -> get value of x
  | Set Indent Value   -- add State (a,s)
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Seq [Expr]         -- seq of expressions
  deriving Show

eval :: Expr -> Table -> (Value,Table)
eval (Lit v) = \t -> (v,t)
eval (Var x) = \t -> 
   case lookup x t of
     Just v  -> (v,t)
     Nothing -> error $ concat ["Value ",x," not defined"]
eval (Set x v) = \t -> (v, (x,v) : t)
eval (Add e f) = \t0 -> 
  let (v, t1) = eval e t0
      (w, t2) =  eval f t1
  in  (v + w, t2)  
eval (Sub e f) = \t0 -> 
  let (v, t1) = eval e t0
      (w, t2) =  eval f t1 
  in  (v - w, t2)
eval (Mul e f) = \t0 -> 
  let (v, t1) = eval e t0
      (w, t2) =  eval f t1 
  in  (v * w, t2)
eval (Seq [e])    = \t0 ->
  let (v,t1) = eval e t0 
  in (v, t1)
eval (Seq []) = \t -> (0,t)
eval (Seq (e:es)) = \t0 ->
  let (v, t1) = eval e t0
      (w, t2) = eval (Seq es) t1
   in (w,t2)

e1 = Add (Lit 3) (Lit 4)
e2 = Sub (Lit 8) (Lit 2)

e3 = Seq [
          Set "x" 2,
          Set "y" 12,
          Set "z" 3,
          Add (Var "x") (Var "y"),
          Mul (Var "z") (Var "x")
          ] 
-- eval e1 []
-- eval e1 [("one",1),("two",2)]

-- eval e3 [("one",1),("two",2)]
-- lookup 7 [ws]

type Name = String
type Amount = Int
data Account = Account {name :: Name, balance :: Amount} deriving Show

withdraw :: Amount -> Account -> Account
withdraw amount (Account name balance) = Account name (balance - amount)

isDrawable :: Account -> Bool
isDrawable (Account _ balance ) = balance > 0

deposit :: Amount -> Account -> Account
deposit amount (Account name balance) = Account name (balance + amount)

--setName -> Name -> Account -> Account
--setName newName account = account {name = newName}

transactions :: Account -> Account
transactions account = 
  let acc1 = withdraw 100 account
      acc2 = if isDrawable acc1 then withdraw 100 acc1 else acc1
  in acc2




newtype Transformer s a =  Transformer {runTransformer :: s -> (a,s)} -- tupple type

-- State Monad!

-- Implement Transformer s  as a monad
instance Functor (Transformer s) where
  fmap  = liftM
--liftM is monad equivalent to fmap 
-- fmap h sa = sa >>= return a

instance Applicative (Transformer s) where
  pure  = return
  (<*>) = ap -- ap is the Monad applicative 

-- ap is the monad equivalente of Applicative

instance Monad (Transformer s) where
--  return a :: Transformer s a
  return a =  Transformer $ \s -> (a,s)
--  (>>=) :: Transformer s a -> (a -> Transformer s b) -> Transformer s a
  sa >>= k = Transformer $ \s0 ->
    let (a, s1) = runTransformer sa s0
        (b, s2) = runTransformer (k a) s1 in
        (b, s2)

put' :: s -> Transformer s ()
put' s = Transformer $ \_ -> ((),s)

get' :: Transformer s s
get' = Transformer $ \s -> (s,s)


-- no need to send input
withdraw' :: Amount -> Transformer Account ()
withdraw' amount  = do
  Account name balance <- get'
  put' (Account name (balance - amount))

isDrawable' :: Transformer Account Bool
isDrawable' = do
  Account _ balance <- get'
  return (balance > 0)

-- state monad get

transaction :: Transformer Account ()
transaction = do
  withdraw' 100
  t <- isDrawable'
  case t of
    True -> do
      withdraw' 100
      t2 <- isDrawable'
      case t2 of 
        True -> withdraw' 100
        False -> return ()
    False -> return ()


------
------ HOME WORK: IMPLEMENT eval using do notation with transformer monad

-- HOMEWORK: please implement 
-- eval :: Expr -> Table -> (Value, Table) as eval :: Expr -> Transformer Table Value using the Transformer monad in do notation.2
evalT :: Expr -> Transformer Table Value
evalT = undefined

-- Transformer or State monad

evalSt :: Expr -> State Table Value
evalSt (Lit v)   = return v -- call the return of state monad
evalSt (Var x) = do
  t <- get 
  case lookup x t of
    Just c  -> return c
    Nothing -> error $ " is not in table" ++ x
evalSt (Set x v) = do modify ((x,v) :); return v 
--  get
--  put $ ((x,v) : t)          -- modify (\t -> (x, v) : t) replaces get and put
-- modify (\t -> (x, v):)
evalSt (Add e f)   = do v <- evalSt e; w <- evalSt f; return (v + w)
evalSt (Sub e f)   = do v <- evalSt e; w <- evalSt f; return (v - w)
evalSt (Mul e f)   = do v <- evalSt e; w <- evalSt f; return (v * w)
evalSt (Div e f)   = do v <- evalSt e; w <- evalSt f; return (div v  w)
evalSt (Seq [e])    = error "No expressions to evaluate"
evalSt (Seq (e:es)) = do _ <- evalSt e; w <- evalSt (Seq es); return w




-- HOMEWORK try to implement 
--eval ::Expr -> Reader e a

