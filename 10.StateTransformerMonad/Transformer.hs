{-# LANGUAGE InstanceSigs, TypeApplications #-}

import System.IO
import Control.Monad
import Control.Monad.State

type Value = Int
type Ident = String
type Table = [(Ident, Value)]
data Expr
  = Lit Value
  | Var Ident
  | Set Ident Value
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr

type Lives = Int
type Secret = Int

-- Monad Transformer
-- Monad Transformer is a type constructor which takes a monad and transforms it into a different monad

-- MaybeT
-- newtype MaybeT m a   = MaybeT { runMaybeT :: m (Maybe a) }

-- StateT
-- newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

-- ReaderT r (StateT s IO) a
-- Monad Stack
-- Reader
-- State
-- IO
--
-- transformers
-- mtl
--  - MonadIO
--  - MonadState
--  - MonadReader

-- instance Monad m => Functor (StateT s m) where
--   fmap = liftM
-- 
-- instance Monad m => Applicative (StateT s m) where
--   pure  = return
--   (<*>) = ap
-- 
-- instance Monad m => Monad (StateT s m) where
--   return :: a -> StateT s m a
--   return a = StateT $ \s -> return (a, s)
-- 
--   (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
--   sma >>= k = StateT $ \s0 -> do
--     (a, s1) <- runStateT sma s0
--     (b, s2) <- runStateT (k a) s1
--     return (b, s2)
-- 
-- lift :: IO a -> StateT s IO a 
-- lift ioa = StateT $ \s -> do
--   a <- ioa
--   return (a, s)
-- 
-- type World a = StateT Lives IO a
--
--
-- what Value is to Type, Type is to Kind
-- 4 :: Int, Int    :: *
--         , Maybe  :: * -> *
--         , Either :: * -> * -> *

play :: Secret -> StateT Lives IO ()
play secret = do
  lives <- get
  case lives < 1 of
    True  -> do
      lift $ putStrLn "Game Over :("
      -- lift function converts the putStrLn type IO () to StateT Lives IO ()
      -- lifts monad to another monad which the type is also part of
    False -> do
      lift $ putStrLn $ show lives ++ " lives left."
      lift $ putStrLn "Please enter the guess."
      guess <- lift $ read <$> getLine
      case compare guess secret of
        EQ -> do
          lift $ putStrLn "Yay, you guessed correct :)\n"
        GT -> do
          lift $ putStrLn "Ah, you guessed too high!\n"
          put (lives - 1)
          --StateT $ \s -> return ((), lives - 1)
          play secret
        LT -> do
          lift $ putStrLn "Ah, you guessed too low.\n"
          put (lives - 1)
          --StateT $ \s -> return ((), lives - 1)
          play secret

getSecret :: IO Int
getSecret = do
  putStrLn "Please enter the secret."
  hSetEcho stdin False
  secret <- read <$> getLine
  hSetEcho stdin True
  putStrLn ""
  return secret

main :: IO ()
main = do
  secret <- getSecret
  runStateT (play secret) 7
  return ()






{-
here is a brief glossary for monad and transformer related terms to make the difference between them clear:

1. State Monad
State Monad is just
newtype State s a = State { runState :: s -> (a, s) }
State s is the monad.

2. State Transformer
State Transformer is just a monad transformer which takes a monad m and infuses it with state manipulation capabilities
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
StateT s is the monad transformer.

3. Transformer Monad
Transformer Monad doesn't exist in the Haskell libraries. it was just the state monad which i implemented for illustrative purposes in the class.

4. Monad Transformer
a monad transformer is any type constructor which, like StateT s, takes a monad and transformes it into something more capable. other examples are ReaderT or MaybeT.

5. Transformer Stack
a stacking of monads over each other using more than one monad transformers to create a mega-monad which has all the capabilities we need to use.

e.g. ReaderT Config (StateT Game IO) ()

6. mtl
a package which implements the monad transformers like StateT and typeclasses related to them for ease of use. note that mtl doesn't implement State as we studied it but rather makes it a special case of StateT like StateT Identity m a where Identity is a monad which embodies no effects.
-}
