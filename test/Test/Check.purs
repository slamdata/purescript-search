module Test.Check
  ( check
  ) where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Either (Either(..))

import Test.Check.Gen (QueryWrapper(..))
import Test.Effects (TEST_EFFECTS)
import Test.StrongCheck (Result(..), quickCheck, (===))
import Text.SlamSearch (mkQuery)
import Text.SlamSearch.Printer (strQuery)

checkFn ∷ QueryWrapper → Result
checkFn (QueryWrapper query) =
  let str = strQuery query in
  case mkQuery str of
    Left _ → Failed (show query <> "\n" <> show str)
    Right res → res === query

check ∷ Eff TEST_EFFECTS Unit
check = quickCheck checkFn
