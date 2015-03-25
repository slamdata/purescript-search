module Text.SlamSearch.Types where

import Data.Semiring.Free

-- | SearchQuery is free semiring on Term
type SearchQuery = Free Term

newtype Term = Term {
  include :: Boolean,
  labels :: [Label],
  predicate :: Predicate
  }

instance showTerm :: Show Term where
  show (Term r) = "(Term {labels: " <> show r.labels <> ", predicate: " <>
                  show r.predicate <> ", include: " <> show r.include <> "})"


instance eqTerm :: Eq Term where
  (==) (Term t) (Term t') =
    t.include == t'.include &&
    t.labels == t'.labels &&
    t.predicate == t'.predicate
  (/=) a b = not $ a == b

data Label = Common String | Meta String

instance showLabel :: Show Label where
  show (Common str) = "(Common " <> show str <> ")"
  show (Meta str) = "(Meta " <> show str <> ")" 


instance eqLabel :: Eq Label where
  (==) (Common s) (Common s') = s == s'
  (==) (Meta s) (Meta s') = s == s'
  (==) _ _ = false
  (/=) a b = not $ a == b

data Predicate =
  Contains Value | Eq Value| Gt Value | Gte Value | Lt Value | Lte Value
  | Ne Value | Like String


instance eqPredicate :: Eq Predicate where
  (==) (Contains v) (Contains v') = v == v'
  (==) (Eq v) (Eq v') = v == v'
  (==) (Gt v) (Gt v') = v == v'
  (==) (Gte v) (Gte v') = v == v'
  (==) (Lte v) (Lte v') = v == v'
  (==) (Lt v) (Lt v') = v == v'
  (==) (Ne v) (Ne v') = v == v'
  (==) (Like s) (Like s') = s == s'
  (==) _ _ = false
  (/=) a b = not $ a == b


instance showPredicate :: Show Predicate where
  show p = case p of
    Contains v -> "(Contains " <> show v <> ")"
    Eq v -> "(Eq " <> show v <> ")"
    Gt v -> "(Gt " <> show v <> ")"
    Gte v -> "(Gte " <> show v <> ")"
    Lt v -> "(Lt " <> show v <> ")"
    Lte v -> "(Lte " <> show v <> ")"
    Ne v -> "(Ne " <> show v <> ")"
    Like v -> "(Like " <> show v <> ")"


data Value
  = Text String
  | Range String String
  | Tag String

instance eqValue :: Eq Value where
  (==) (Text s) (Text s') = s == s'
  (==) (Tag s) (Tag s') = s == s'
  (==) (Range b u) (Range b' u') = b == b' && u == u'
  (==) _ _ = false
  (/=) a b = not $ a == b

instance showValue :: Show Value where
  show v = case v of
    Text s -> "(Text " <> show s <> ")"
    Tag s -> "(Tag " <> show s <> ")"
    Range b u -> "(Range " <> show b <> " " <> show u <> ")"
