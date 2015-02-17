module Test.Check where

import Data.Either
import Test.StrongCheck

import Text.SlamSearch.Parser
import Text.SlamSearch.Printer
import Test.Check.Gen

checkFn :: SearchQuery -> Result
checkFn query =
  let str = prettyQuery query in
  case parseSearchQuery str of
    Left _ -> Failed (show query <> "\n" <> show str)
    Right result -> 
      result === query

check =
  quickCheck checkFn
