module DataTypes where

  data Student = Student { 
                           name         :: String 
                         , grades       :: [Int]
                         , courses      :: [String]
                         , certificates :: [String]
                         } deriving Show

  data Batches a = Batch a (Batches a) | Closes deriving Show


  learner1 :: Student
  learner1 = Student { name         = "Roberto"
                     , grades       = [83,74,99,92]
                     , courses      = ["FOB", "CDP", "HDP", "CSA"]
                     , certificates = []
                     }
  learner2 = Student { name         = "Brian"
                     , grades       = [95,98,92,100]
                     , courses      = ["HDP","CSA"]
                     , certificates = []
                     }
  learner3 = Student { name         = "Carlos"
                     , grades       = [83,74,79,80]
                     , courses      = ["FOB", "CDP"]
                     , certificates = []
                     }
  
  learner4 = Student { name         = "Alice"
                     , grades       = [95,98,92,100]
                     , courses      = ["HDP","CSA"]
                     , certificates = []
                     }
  learner5 = Student { name         = "Clarice"
                     , grades       = [83,74,79,80]
                     , courses      = ["FOB", "CDP"]
                     , certificates = []
                     }
  learner6 = Student { name         = "Miguel"
                     , grades       = [95,98,92,100]
                     , courses      = ["HDP","CSA"]
                     , certificates = []
                     }
  learner7 = Student { name         = "Hanibal"
                     , grades       = [83,74,79,80]
                     , courses      = ["FOB", "CDP"]
                     , certificates = []
                     }

  
  batch100 = Batch learner1 $ Batch learner2 $ Batch learner3 Closes

  batch101 = Batch learner4 $ Batch learner5 Closes

  batch102 = Batch learner6 $ Batch learner7 Closes



  instance Semigroup (Batches a) where     
    Closes         <> Closes           = Closes
    Batch x xs     <> Closes           = Batch x xs
    Closes         <> Batch x xs       = Batch x xs
    Batch x xs     <> Batch y Closes   = Batch y $ Batch x xs  
    Batch x Closes <> Batch y ys       = Batch x $ Batch y ys
    Batch x xs     <> Batch y ys       = Batch x $ xs <> Batch y ys







  instance Monoid (Batches a) where
    mempty           = Closes
    mappend mempty y = y

  instance Functor Batches where
    fmap f Closes       = Closes
    fmap f (Batch s y)  = Batch (f s) (fmap f y)

  instance Applicative Batches where
    pure x = Batch x Closes
    --(Batch f Closes) <*> Batch x (Batch y Closes)
    (Batch f Closes) <*> Batch s y = Batch (f s) (fmap f y)
    _ <*> Closes              = Closes

   --(<*>) :: f (a -> b) -> f a -> f b
   --           Just (+3)    Just 2     Just 5
   --           Batch        batch100   Batch Student Closes
   -- avgGradeComprade :: Ord a => (a -> a -> Bool) -> Batches Student -> Batches Student -> Bool

--   rankOnBatch :: Batches Student -> Student

-- => (Batch Student Closes)

   --rankBetween Batches => op1.  Batches (The whole winning Batch)
   --                       op2.  Batch Student Closes