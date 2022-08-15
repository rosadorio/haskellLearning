module MoreTypeClasses where
import Data.Int

--Custom Data Type

-- Type constructor  =>   data Color a
-- Data constructor  =>   C a
-- a is a Type variable
--            0      1      2        3
data Mood = Grumpy | Sad | Happy | Extatic deriving (Show,Eq,Ord)
data ColorNames a = Red a| Green a| Blue a| White a| Black a deriving Show
data Color a = C a deriving Show

x :: Color Int
x = C 200

y :: Color String
y = C "Blue"

col :: Color (ColorNames (Color (ColorNames Mood)))
col = C (Red (C (Blue Sad)))


data Complex a b = SomeDataContruct a | SomeOtherDataconstruct b deriving Show
newtype Col a = Col a  deriving Show

z :: Col Char
z = Col 'R'

--instance Eq Mood where
-- Grumpy == Grumpy = True
-- Grumpy == Sad    = False


--instance Ord Mood where
-- Grumpy > Sad     = False
-- Grumpy < Sad     = True
