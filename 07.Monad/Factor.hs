-- factors
factors :: Int -> [Int]
factors n = [ x | x <- [1..n], mod n x == 0 ]

-- primes
primes :: Int -> [Int]
--primes n = [ x | x <- [1..n], factors x == [1,x]]
primes n = do
  x <- [1..n]
  if factors x == [1,x] then [x] else []
 
-- List compreension is actually a monadic operator

-- do i <- [1..10]; j <- [11..20]; return (i,j)
-- [(i,j) | i <- [1..10]; j <- [11..20] ] 

-- filter in terms of map
-- concat $ map (\x -> if odd x then [x] else []) [1,2,3]


data Box a b = Box a b
-- :k Box
-- * -> * -> *
-- Box (2:: Int) 'a'

-- instance Functor Box where (Fails due to kind )
instance Functor (Box a) where
  fmap _ _ = undefined 


data Box2 a = A a | B a | C a

instance Functor Box2 where
  fmap h (A a)  = A (h a)
  fmap h (B a)  = B (h a)
  fmap h (C a)  = C (h a)

instance Applicative Box2 where
  pure a = A a
  A f <*> A v = A (f v)
  A f <*> B v = B (f v)
  A f <*> C v = C (f v)

  