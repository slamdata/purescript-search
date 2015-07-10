module Test.Check where

import Prelude
import Data.Either
import Test.StrongCheck
import Text.SlamSearch (mkQuery)
import Text.SlamSearch.Printer (strQuery) 
import Text.SlamSearch.Types
import Test.Check.Gen
import Data.Semiring.Free

checkFn :: QueryWrapper -> Result
checkFn (QueryWrapper query) = 
  let str = strQuery query in
  case mkQuery str of
    Left _ -> Failed (show query <> "\n" <> show str)
    Right res -> res === query

check = quickCheck checkFn

