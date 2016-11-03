module Text.SlamSearch.Types
  ( SearchQuery(..)
  , Term(..)
  , Label(..)
  , Predicate(..)
  , Value(..)
  ) where

import Prelude

import Data.List (List(..), fromFoldable)
import Data.Semiring.Free (Free)
import Data.String as Str

import Test.StrongCheck.Arbitrary as SCA
import Test.StrongCheck.Gen as Gen

-- | SearchQuery is free semiring on Term where
-- | `AND` operation is `*` and `OR` is `+`
type SearchQuery = Free Term

newtype Term = Term
  { include ∷ Boolean
  , labels ∷ List Label
  , predicate ∷ Predicate
  }

instance showTerm ∷ Show Term where
  show (Term r) =
    "(Term {labels: "
    <> show r.labels <> ", predicate: "
    <> show r.predicate <> ", include: "
    <> show r.include <> "})"

derive instance eqTerm ∷ Eq Term

instance arbTerm ∷ SCA.Arbitrary Term where
  arbitrary = do
    include ← SCA.arbitrary
    labels ← fromFoldable <$> (SCA.arbitrary ∷ Gen.Gen (Array Label))
    predicate ← SCA.arbitrary
    pure $ Term { include, labels, predicate }

-- | Label type
-- | `"foo:bar" -→ Common (Text "foo")`,
-- | `"@foo:bar" -→ Meta (Text "foo")`
data Label = Common String | Meta String

instance showLabel ∷ Show Label where
  show (Common str) = "(Common " <> show str <> ")"
  show (Meta str) = "(Meta " <> show str <> ")"

derive instance eqLabel ∷ Eq Label

instance arbLabel ∷ SCA.Arbitrary Label where
  arbitrary = do
    constructor ← Gen.elements Common (Cons Meta Nil)
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

derive instance eqPredicate ∷ Eq Predicate

instance showPredicate ∷ Show Predicate where
  show p = case p of
    Contains v → "(Contains " <> show v <> ")"
    Eq v → "(Eq " <> show v <> ")"
    Gt v → "(Gt " <> show v <> ")"
    Gte v → "(Gte " <> show v <> ")"
    Lt v → "(Lt " <> show v <> ")"
    Lte v → "(Lte " <> show v <> ")"
    Ne v → "(Ne " <> show v <> ")"
    Like v → "(Like " <> show v <> ")"
    Range v vv → "(Range " <> show v <> show vv <> ")"

instance arbPredicate ∷ SCA.Arbitrary Predicate where
  arbitrary = do
    val ← SCA.arbitrary
    val' ← SCA.arbitrary
    str ← genName
    Gen.elements (Contains val) $ fromFoldable
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
-- | `"foo:bar" -→ Text "bar"`
-- | `"foo:#bar" -→ Tag "bar"`
data Value = Text String | Tag String

genGenName ∷ Array Char → Gen.Gen String
genGenName strin = do
  len ← Gen.chooseInt 1 5
  go len ""
  where
  go 0 acc = pure acc
  go len acc = do
    ch ← map Str.singleton $ Gen.allInArray validChars
    go (len - 1) (ch <> acc)

validChars ∷ Array Char
validChars =
  Str.toCharArray
  $ "abcdefghijklmnopqrstuvwxyz"
  <> "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  <> "01234567890"

genName ∷ Gen.Gen String
genName = genGenName validChars

instance arbitraryValue ∷ SCA.Arbitrary Value where
  arbitrary = do
    str ← genName
    Gen.elements (Text str) $ fromFoldable [ Tag str ]

derive instance eqValue ∷ Eq Value

instance showValue ∷ Show Value where
  show =
    case _ of
      Text s → "(Text " <> show s <> ")"
      Tag s → "(Tag " <> show s <> ")"
