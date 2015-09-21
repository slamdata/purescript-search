module Test.Check.Gen
  ( QueryWrapper(..)
  ) where

import Prelude
import Test.StrongCheck.Gen
import Test.StrongCheck hiding (test)

import qualified Data.String as Str
import qualified Data.Char as Ch
import Data.Semiring.Free
import Data.List (toList, List(..))

import Text.SlamSearch.Types
import Data.Foldable (foldl)

-- helper type not to add orphan instances of Arbitrary on Free a
newtype QueryWrapper = QueryWrapper SearchQuery

instance arbQueryWrapper :: Arbitrary QueryWrapper where
  arbitrary = do
    k <- chooseInt 1.0 10.0
    lst <- vectorOf k $ free <$> arbitrary
    pure <<< QueryWrapper $ foldl (*) one lst

