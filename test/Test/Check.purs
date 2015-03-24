module Test.Check where

import Data.Either
import Test.StrongCheck
import Text.SlamSearch (mkQuery)
import Text.SlamSearch.Printer (strQuery) 
import Text.SlamSearch.Types
import Test.Check.Gen
import Data.Semiring.Free

checkFn :: QueryWrapper -> Result
checkFn (QueryWrapper query) =
  let str = strQuery query
      res = mkQuery str
  in res === query
  

check = quickCheck checkFn
