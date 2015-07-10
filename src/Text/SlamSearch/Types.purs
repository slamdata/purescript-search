module Text.SlamSearch.Types where

import Prelude
import Data.Semiring.Free (Free())
import Data.List (List())

-- | SearchQuery is free semiring on Term
type SearchQuery = Free Term

newtype Term = Term {
  include :: Boolean,
  labels :: List Label,
  predicate :: Predicate
  }

instance showTerm :: Show Term where
  show (Term r) = "(Term {labels: " <> show r.labels <> ", predicate: " <>
                  show r.predicate <> ", include: " <> show r.include <> "})"


instance eqTerm :: Eq Term where
  eq (Term t) (Term t') =
    t.include == t'.include &&
    t.labels == t'.labels &&
    t.predicate == t'.predicate

data Label = Common String | Meta String

instance showLabel :: Show Label where
  show (Common str) = "(Common " <> show str <> ")"
  show (Meta str) = "(Meta " <> show str <> ")" 


instance eqLabel :: Eq Label where
  eq (Common s) (Common s') = s == s'
  eq (Meta s) (Meta s') = s == s'
  eq _ _ = false

data Predicate
  = Contains Value
  | Eq Value
  | Gt Value
  | Gte Value
  | Lt Value
  | Lte Value
  | Ne Value
  | Like String


instance eqPredicate :: Eq Predicate where
  eq (Contains v) (Contains v') = v == v'
  eq (Eq v) (Eq v') = v == v'
  eq (Gt v) (Gt v') = v == v'
  eq (Gte v) (Gte v') = v == v'
  eq (Lte v) (Lte v') = v == v'
  eq (Lt v) (Lt v') = v == v'
  eq (Ne v) (Ne v') = v == v'
  eq (Like s) (Like s') = s == s'
  eq _ _ = false

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
  eq (Text s) (Text s') = s == s'
  eq (Tag s) (Tag s') = s == s'
  eq (Range b u) (Range b' u') = b == b' && u == u'
  eq _ _ = false

instance showValue :: Show Value where
  show v = case v of
    Text s -> "(Text " <> show s <> ")"
    Tag s -> "(Tag " <> show s <> ")"
    Range b u -> "(Range " <> show b <> " " <> show u <> ")"
