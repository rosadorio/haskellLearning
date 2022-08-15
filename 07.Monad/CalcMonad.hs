module Calc where

type Value = Float

data Expr = Sum Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Sqrt Expr| Lit Value deriving Show

eval :: Expr -> Value
eval (Lit v)    = v
eval (Sum e f)  = eval e + eval f
eval (Sub e f)  = eval e - eval f
eval (Mul e f)  = eval e * eval f
eval (Div e f)  = (eval e) / (eval f)
eval (Sqrt e)   = sqrt $ eval e

--
expr = Sub (Sum (Sub (Lit 2) (Lit 13)) (Mul (Lit 4) (Lit 5))) (Div (Lit 6) (Lit 3))
expr1 = Sub (Sum (Sub (Lit 2.0) (Lit 13.0)) (Mul (Lit 4.0) (Lit 5.0))) (Div (Lit 6.0) (Lit 0.0))
expr2 = Sub (Lit 4) (Lit 3)
expr3 = Sum (Sqrt (Lit 2)) (Sqrt (Lit 7))


-- Maybe Monad
safeDiv :: Value -> Value -> Maybe Value
safeDiv _ 0   = Nothing
safeDiv a b   = Just (a / b)

expSqrt :: Float -> [Float]
expSqrt x = map ($ (sqrt x)) [id, negate]


evalList :: Expr -> [Value]
evalList (Lit v)    = [v] 
evalList (Sum e f)  = app (+) (evalList e) (evalList f)  -- implementation with our appdefubutuib
evalList (Sub e f)  = evalList e >>= \v -> evalList f >>= \w -> return (v + w) -- monad notation
evalList (Mul e f)  = app (*) (evalList e) (evalList f)
evalList (Div e f)  = app (/) (evalList e) (evalList f)
evalList (Sqrt e)   = evalList e >>= \v -> expSqrt v
       
-- ex: evalList expr3


--  let vs = evalList e
--      ws = evalList f in
--      ap (+) vs ws
-- pure (+) <*> vs <*> ws


-- explicit applicative definition
app :: (Value -> Value -> Value) -> [Value] -> [Value] -> [Value]
app op vs ws = go fs 
  where  fs = fmap op vs
         go []     = []
         go (g:gs)    = fmap g ws ++ go gs

{-
evalMaybe :: Expr -> Maybe Value
evalMaybe (Lit v)    = Just v
evalMaybe (Sum e f)  = 
  case eval e of 
    Nothing -> Nothing 
    Just v  -> case eval f of
      Nothing -> Nothing
      Just w  -> Just (v + w)
evalMaybe (Sub e f)  = 
  case eval e of
    Nothing -> Nothing 
    Just v  -> case eval f of
      Nothing -> Nothing
      Just w  -> Just (v - w)
evalMaybe (Mul e f) =
  case eval e of
    Nothing -> Nothing 
    Just v  -> case eval f of
      Nothing -> Nothing
      Just w  -> Just (v * w)
evalMaybe (Div e f) = 
  case eval e of
    Nothing -> Nothing 
    Just v  -> case eval f of
      Nothing -> Nothing
      Just w  -> safeDiv v w
-}

--
--return :: a -> Maybe a
--return = Just

--(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--ma >> k = 
--  case ma of
--    Nothing -> Nothing
--    Just a  -> k a

-- INFIX BIND NOTATION
evalMonad :: Expr -> Maybe Value
evalMonad (Lit v) = return v
evalMonad (Sum e f) =
  evalMonad e >>= (\v -> evalMonad f >>= (\w ->  return (v + w)))
evalMonad (Sub e f) =
  evalMonad e >>= \v -> 
  evalMonad f >>= \w -> 
  return (v-w)
evalMonad (Mul e f) =
  evalMonad e >>=  \v -> 
  evalMonad f >>= \w -> 
  return (v*w)
evalMonad (Div e f) =
  evalMonad e >>= \v -> 
  evalMonad f >>= \w -> 
  safeDiv v w
--evalMonad (Sqrt v) 
-- DO NOTATION
evalMonad' :: Expr -> Maybe Value
evalMonad' (Lit v) = return v
evalMonad' (Sum e f) = do
  v <- evalMonad' e
  w <- evalMonad' f
  return $ v + w
evalMonad' (Sub e f) =  do
  v <- evalMonad' e
  w <- evalMonad' f
  return (v-w)
evalMonad' (Mul e f) = do
  v <- evalMonad' e
  w <- evalMonad' f
  return (v*w)
evalMonad' (Div e f) = do
  v <- evalMonad' e
  w <- evalMonad' f
  safeDiv v w



-- Another Example
a ::  Int -> Maybe Int
a x = if x < 7 then return (x + 2) else Nothing

b ::  Int -> Maybe Int
b x = if x < 12 then return (x * 2) else Nothing

c ::  Int -> Maybe Int
c x = if x < 4 then return (x ^ 2) else Nothing


h :: Int -> Int -> Int -> Maybe Int
h x y z = case a x of
  Nothing -> Nothing
  Just u  -> case b y of
    Nothing -> Nothing
    Just v  -> case c z of
      Nothing -> Nothing
      Just w  -> Just (u + v + w)

h' :: Int -> Int -> Int -> Maybe Int
h' x y z = do
  u <- a x
  v <- b y
  w <- c z
  return (u+v+w)

h'':: Int -> Int -> Int -> Maybe Int
h'' x y z =
  a x >>= \u ->
  b y >>= \v ->
  c z >>= \w ->
  return $ u + v + w

-- Go through the monadic implementation of evalMonad
--  Try to implement eval :: Expr -> Either String Value
evalEither :: Expr -> Either String Value
evalEither (Lit v) = return v
evalEither (Sum e f) =
  evalEither e >>= (\v -> evalEither f >>= (\w ->  return (v + w)))
evalEither (Sub e f) =
  evalEither e >>= \v -> 
  evalEither f >>= \w -> 
  return (v-w)
evalEither (Mul e f) =
  evalEither e >>=  \v -> 
  evalEither f >>= \w -> 
  return (v*w)
evalEither (Div e f) =
  evalEither e >>= \v -> 
  evalEither f >>= \w -> 
  safeDiv' v w


-- Either Div
safeDiv' :: Value -> Value -> Either String Value
safeDiv' _ 0   = Left "Cant divide by zero"
safeDiv' a b   = Right (a / b)

-- Instance of the Monad List
l1 = [1,2,3]
l2 = [4,2]
d = (\x y -> (x,y))  <$> a <*> b

-- non deterministic computation
firstname = ["Eu","Tu","Nos","Vos","Eles"]
lastname  = ["Black","White"]

poss=(\f l -> f ++ " " ++l) <$> firstname <*> lastname

