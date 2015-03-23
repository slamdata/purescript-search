module Text.SlamSearch.Types where

import Data.Semiring.Free

-- | SearchQuery is free semiring on Term
type SearchQuery = Free Term

type Term = {
  include :: Boolean,
  labels :: [Label],
  predicate :: Predicate
  }

data Label = Common String | Meta String

data Predicate =
  Contains Value | Eq Value| Gt Value | Gte Value | Lt Value | Lte Value
  | Ne Value | Like String

data Value
  = Text String
  | Range String String
  | Tag String
