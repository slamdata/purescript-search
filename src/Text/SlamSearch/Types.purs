module Text.SlamSearch.Types
  ( SearchQuery(..)
  , Term(..)
  , Label(..)
  , Predicate(..)
  , Value(..)
  ) where

import Prelude
import Data.Semiring.Free (Free())
import Data.List (List(..), toList)
import Data.String as Str
import Data.Char as Ch
import Test.StrongCheck as SC
import Test.StrongCheck.Gen as SC

-- | SearchQuery is free semiring on Term where
-- | `AND` operation is `*` and `OR` is `+`
type SearchQuery = Free Term

newtype Term = Term
  { include :: Boolean
  , labels :: List Label
  , predicate :: Predicate
  }

instance showTerm :: Show Term where
  show (Term r) =
    "(Term {labels: "
    <> show r.labels <> ", predicate: "
    <> show r.predicate <> ", include: "
    <> show r.include <> "})"

instance eqTerm :: Eq Term where
  eq (Term t) (Term t') =
       t.include == t'.include
    && t.labels == t'.labels
    && t.predicate == t'.predicate

instance arbTerm :: SC.Arbitrary Term where
  arbitrary = do
    r <- {include: _, labels: _, predicate: _}
         <$> SC.arbitrary
         <*> (toList <$> (SC.arbitrary :: SC.Gen (Array Label)))
         <*> SC.arbitrary
    pure $ Term r

-- | Label type
-- | `"foo:bar" --> Common (Text "foo")`,
-- | `"@foo:bar" --> Meta (Text "foo")`
data Label = Common String | Meta String

instance showLabel :: Show Label where
  show (Common str) = "(Common " <> show str <> ")"
  show (Meta str) = "(Meta " <> show str <> ")"

instance eqLabel :: Eq Label where
  eq (Common s) (Common s') = s == s'
  eq (Meta s) (Meta s') = s == s'
  eq _ _ = false

instance arbLabel :: SC.Arbitrary Label where
  arbitrary = do
    constructor <- SC.elements Common (Cons Meta Nil)
    constructor <$> genName

data Predicate
  = Contains Value
  | Eq Value
  | Gt Value
  | Gte Value
  | Lt Value
  | Lte Value
  | Ne Value
  | Like String
  | Range Value Value

instance eqPredicate :: Eq Predicate where
  eq (Contains v) (Contains v') = v == v'
  eq (Eq v) (Eq v') = v == v'
  eq (Gt v) (Gt v') = v == v'
  eq (Gte v) (Gte v') = v == v'
  eq (Lte v) (Lte v') = v == v'
  eq (Lt v) (Lt v') = v == v'
  eq (Ne v) (Ne v') = v == v'
  eq (Like s) (Like s') = s == s'
  eq (Range r rr) (Range r' rr') =
       r == r'
    && rr == rr'
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
    Range v vv -> "(Range " <> show v <> show vv <> ")"

instance arbPredicate :: SC.Arbitrary Predicate where
  arbitrary = do
    val <- SC.arbitrary
    val' <- SC.arbitrary
    str <- genName
    SC.elements (Contains val) $ toList
      [ Eq val
      , Gt val
      , Gte val
      , Lt val
      , Lte val
      , Ne val
      , Like str
      , Range val val'
      ]

-- | Value type
-- | `"foo:bar" --> Text "bar"`
-- | `"foo:#bar" --> Tag "bar"`
data Value = Text String | Tag String

genGenName :: String -> SC.Gen String
genGenName strin = do
  len <- SC.chooseInt 1.0 5.0
  go len ""
  where
  go 0 acc = pure acc
  go len acc = do
    ch <- Str.fromChar <$> SC.elements (Ch.fromCharCode 65)
          (toList $ Str.toCharArray validChars)
    go (len - 1) (ch <> acc)

validChars :: String
validChars =
  "bcdefghijklmnopqrstuvwxyz"
  <> "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  <> "01234567890"

genName :: SC.Gen String
genName = genGenName validChars

instance arbitraryValue :: SC.Arbitrary Value where
  arbitrary = do
    str <- genName
    SC.elements (Text str) $ toList [ Tag str ]

instance eqValue :: Eq Value where
  eq (Text s) (Text s') = s == s'
  eq (Tag s) (Tag s') = s == s'
  eq _ _ = false

instance showValue :: Show Value where
  show v = case v of
    Text s -> "(Text " <> show s <> ")"
    Tag s -> "(Tag " <> show s <> ")"
