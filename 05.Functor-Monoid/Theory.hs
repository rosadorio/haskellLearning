

{-

Semigroup > Monoid > Functor > Applicative > Monad

Each of these algebras is more powerful than the last but the general concept is the same:
Abstract out a common pattern, make certain it follows some laws, give it a name 


Functor is a pattern of mapping over a structure (fmap)
A functor is a way to aply a function over a structure that we dont want to alter. Apply the function inside the structure and leave the structure alone. No elements are removed or added only transformed (just like map)


*Functor definition*

Class Functor f where:
  fmap :: (a -> b) -> f a -> f b 
  (<$>) infix alias for fmap
the functor can only be instanced for types 'f' (* -> *)

Functor LAWS:
  1. Identity
     fmap id == id

  2. Composition
     fmap (f . g) = fmap f . fmap g

  3. Structure Preservation


It is only a real functor when these 3 laws are guaranteed 



*Applicative* 

Applicative can be defined as a moinoidal functor. Allows for function application over many structures. With Applicative the function we are applying also is also embeded in structure

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

we can define fmap from applicative instance
  fmap f x = pure f <*> x


Example with Maybe Applicative

instance Applicative Maybe where:
  pure              = Just 
  Nothing <*> _     = Nothing
  _ <*> Nothing     = Nothing
  Just f <*> Just a = Just (f a)


Applicative Laws:

1. Identity
  pure id  <*> v = v

2. Composition
  pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

3. Homomorphism: structure preservation between two algebraic structures. Function application should preserve the structure
  pure f <*> pure x = pure (f x)

4. Interchange
  u <*> pure y = pure ($ y) <*> u





-}

data Three a b c = Three a b c deriving Show

x = Three 1 2 3

instance Functor (Three a b) where
   fmap f (Three a b c) = Three a b (f c)


-- create an Applicative instance for:

-- 1.
data Pair a = Pair a a deriving Show

y = Pair "One" "Two"

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a                     = Pair a a
  (Pair f1 f2) <*> (Pair c d)  = Pair (f1 c)  (f2 d)
  