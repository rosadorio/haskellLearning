import Prelude hiding (return, (>>=))

data Box a = Box { unbox :: a} deriving Show

return :: a -> Box a
return a = Box a

(>>=) :: Box a -> (a -> Box b) -> Box b
ba >>= k  = k (unbox ba) 

x :: Box String
x = Box "onnne"
y :: Box String
y = Box "twwwo"

greet :: Box String
greet = 
  x >>= \a ->
  y >>= \b ->
  return ("hello biatches, "++ a ++ " " ++ b)

--
data Box2 a b = Box2 a b deriving Show
mkBox :: a -> b -> Box2 a b
mkBox a b = Box2 a b

