module EitherMonad where

import Data.Either

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
safeDiv :: Int -> Int -> Either String Int
safeDiv _ 0 = Left "You cant divide by zero"
safeDiv a b = Right (a `div` b)

divE = "You cant divide by zero"

safeEval :: Expr -> Either String Int
safeEval (Con a) = Right a
safeEval (Add a b) = case safeEval a of
   Left str -> Left "failed to evaluate"
   Right a  -> case safeEval b of
        Left str -> Left "failed to evaluate"
        Right b  -> Right (a + b)
safeEval (Mult a b) = case safeEval a of
   Left str -> Left "failed to evaluate"
   Right a  -> case safeEval b of
        Left str -> Left "failed to evaluate"
        Right b  -> Right (a * b)
safeEval (Div a b) = case safeEval a of
   Left str -> Left "failed to evaluate"
   Right a  -> case safeEval b of
        Left str -> Left "failed to evaluate"
        Right b  -> safeDiv a b


-- Either Monad using anonymoys functions
safeEval' :: Expr -> Either String Int
safeEval' (Con v) = Right v
safeEval' (Add e f) = safeEval' e >>= \e' -> safeEval' f >>= \f' -> return (e' + f')
safeEval' (Mult e f) = safeEval' e >>= \e' -> safeEval' f >>= \f' -> return (e' * f')
safeEval' (Div e f) = safeEval' e >>= \e' -> safeEval' f >>= \f' -> e' `safeDiv` f'


-- same with 'do' notation and ti seems like imperative languages
safeEval'' :: Expr -> Either String Int
safeEval'' (Con v) = Right v
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

