module InferMonads where

data Expr = Con Int 
          | Add Expr Expr
          | Mult Expr Expr
          | Div Expr Expr
          deriving Show
    
ctd :: Expr 
ctd = Add (Mult (Div (Con 12) (Con 4)) (Add (Con 2) (Con 1))) (Con 2) 
-- equivalent to 
-- (12/4 * (2+1)) + 2 
ctd2 :: Expr 
ctd2 = Add (Mult (Div (Con 12) (Con 0)) (Add (Con 2) (Con 1))) (Con 2) 


eval :: Expr -> Int
eval (Con a) = a
eval (Add a b) = eval a + eval b
eval (Mult a b) = eval a * eval b
eval (Div a b) = eval a `div` eval b 

-- eval ctd = 11

-- What happens if we divide by zero?

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (a `div` b)

safeEval :: Expr -> Maybe Int
safeEval (Con a) = Just a
safeEval (Add a b) = case safeEval a of
   Nothing -> Nothing
   Just a  -> case safeEval b of
        Nothing -> Nothing
        Just b  -> Just (a + b)
safeEval (Mult a b) = case safeEval a of
   Nothing -> Nothing
   Just a  -> case safeEval b of
        Nothing -> Nothing
        Just b  -> Just (a * b)
safeEval (Div a b) = case safeEval a of
   Nothing -> Nothing
   Just a  -> case safeEval b of
        Nothing -> Nothing
        Just b  -> safeDiv a b


--   ---[]----
--   |       |
--Nothing  --[]--
--         |    |
--     Nothing [ ] 


-- Monad
-- Apply operator type (aka shovel it in)
  -- (>>=) :: m a -> (a -> m b) -> m b
  --        Maybe a  -> (a -> Maybe b) -> Maybe b
  


-- m a >>= k = case m a of 
--               Nothing -> Nothing
--               Just a -> k a

-- retun :: a -> m a


safeEval' :: Expr -> Maybe Int
safeEval' (Con v) = Just v
safeEval' (Add e f) = safeEval' e >>= \e' -> safeEval' f >>= \f' -> return (e' + f')
safeEval' (Mult e f) = safeEval' e >>= \e' -> safeEval' f >>= \f' -> return (e' * f')
safeEval' (Div e f) = safeEval' e >>= \e' -> safeEval' f >>= \f' -> e' `safeDiv` f'

safeEval'' :: Expr -> Maybe Int
safeEval'' (Con v) = Just v
safeEval'' (Add e f) = do
                        e' <- safeEval'' e
                        f' <- safeEval'' f
                        return (e' + f')
safeEval'' (Mult e f) = do
                        e' <- safeEval'' e
                        f' <- safeEval'' f
                        return (e' * f')
safeEval'' (Div e f) = do
                        e' <- safeEval'' e
                        f' <- safeEval'' f
                        e' `safeDiv` f'


ioEval :: Expr -> IO Int
ioEval (Con v) = return v
ioEval (Add e f) = putStrLn ("Adding " ++ show e ++ " plus " ++ show f) >>
                   ioEval e >>= \e' -> ioEval f >>= \f' -> return (e' + f')
ioEval (Mult e f) = putStrLn ("Multiplying " ++ show e ++ " and " ++ show f) >>
                   ioEval e >>= \e' -> ioEval f >>= \f' -> return (e' * f')
ioEval (Div e f) = ioEval f >>= \f' ->
                     case f' of
                       0          -> putStrLn "You know you can't divide by zero" >> return 0
                       otherwise  ->
                         putStrLn ("Dividing " ++ show e ++ " by " ++ show f) >>
                         ioEval e >>= \e' -> ioEval f >>= \f' -> return (e' `div` f') 


-- (>>) :: m a -> m b -> m b
  -- return :: a -> m a
