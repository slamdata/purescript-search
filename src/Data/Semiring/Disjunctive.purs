module Data.Semiring.Disjunctive where

-- | Boolean semiring in disjunctive normal form
-- |
-- | ```purescript
-- | Disjunctive x + Disjunctive y = Disjunctive (x || y)
-- | Disjunctive x * Disjunctive y = Disjunctive (x && y) 
-- | zero :: Disjunctive = Disjunctive false
-- | one :: Disjunctive = Disjunctive true
-- | ```

newtype Disjunctive = Disjunctive Boolean

runDisjunctive :: Disjunctive -> Boolean
runDisjunctive (Disjunctive a) = a 

instance eqDisjunctive :: Eq Disjunctive where
  (==) (Disjunctive a) (Disjunctive b) = a == b
  (/=) (Disjunctive a) (Disjunctive b) = a /= b

instance ordDisjunctive :: Ord Disjunctive where
  compare (Disjunctive a) (Disjunctive b) = compare a b

instance showDisjunctive :: Show Disjunctive where
  show (Disjunctive a) = "Disjunctive(" <> show a <> ")"

instance boolLikeDisjunctive :: BoolLike Disjunctive where
  (||) (Disjunctive a) (Disjunctive b) = Disjunctive $ a || b
  (&&) (Disjunctive a) (Disjunctive b) = Disjunctive $ a && b
  not (Disjunctive a) = Disjunctive $ not a 

instance semiringDisjunctive :: Semiring Disjunctive where
  one = Disjunctive true
  zero = Disjunctive false
  (*) (Disjunctive a) (Disjunctive b) = Disjunctive $ a && b
  (+) (Disjunctive a) (Disjunctive b) = Disjunctive $ a || b
