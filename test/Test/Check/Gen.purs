module Test.Check.Gen
  ( QueryWrapper(..)
  ) where

import Prelude

import Data.Foldable (foldl)
import Data.Semiring.Free (free)

import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen (vectorOf, chooseInt)
import Text.SlamSearch.Types (SearchQuery)

-- helper type not to add orphan instances of Arbitrary on Free a
newtype QueryWrapper = QueryWrapper SearchQuery

instance arbQueryWrapper ∷ Arbitrary QueryWrapper where
  arbitrary = do
    k ← chooseInt 1 10
    lst ← vectorOf k $ free <$> arbitrary
    pure $ QueryWrapper $ foldl (*) one lst
