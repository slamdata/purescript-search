module Test.Check
  ( check
  ) where

import Prelude

import Control.Monad.Eff (Eff())
import Data.Either
import Test.StrongCheck
import Text.SlamSearch (mkQuery)
import Text.SlamSearch.Printer (strQuery)
import Test.Check.Gen
import Test.Effects (TEST_EFFECTS())

checkFn :: QueryWrapper -> Result
checkFn (QueryWrapper query) =
  let str = strQuery query in
  case mkQuery str of
    Left _ -> Failed (show query <> "\n" <> show str)
    Right res -> res === query

check :: Eff TEST_EFFECTS Unit
check = quickCheck checkFn
