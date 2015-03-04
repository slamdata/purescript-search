module Test.Check.Gen where

import Data.String
import Data.Char
import Data.String.Regex (test, regex, noFlags)
import Data.Array (elemIndex)

import Test.StrongCheck hiding (test)
import Test.StrongCheck.Gen 

import Text.SlamSearch.Parser
import Text.SlamSearch.Parser.Terms
import Text.SlamSearch.Parser.Values
import Text.SlamSearch.Parser.Tokens (keyChars)
import Text.SlamSearch.Printer

validChars :: String
validChars = "bcdefghijklmnopqrstuvwxyz" <>
             "ABCDEFGHIJKLMNOPQRSTUVWXYZ" <>
             "01234567890"

genGenName :: String -> Gen String
genGenName strin = do
  len <- chooseInt 1 5
  genName' len ""
  where genName' len acc =
          case len of
            0 -> return acc
            n -> do
              ch <- fromChar <$> elements (fromCharCode 65)
                    (toCharArray validChars)
                    
              genName' (len - 1) (ch <> acc)
              
genName :: Gen String
genName = genGenName validChars 

genGlob :: Gen String
genGlob = ((<>) "*") <$> genGenName (validChars <> "*?")

instance arbValue :: Arbitrary Value where
  arbitrary = do
    str <- genName
    str' <- genName
    glob <- genGlob
    elements (TextVal str) [
      RangeVal str str',
      Glob glob, 
      Tag str
      ]



instance arbPredicate :: Arbitrary Predicate where
  arbitrary = do
    constructor <- elements ContainsPredicate [
      EqPredicate,
      GtPredicate,
      GtePredicate,
      LtPredicate,
      LtePredicate,
      NePredicate,
      LikePredicate
      ]
    constructor <$> arbitrary
      

instance arbLabel :: Arbitrary Label where
  arbitrary = do
    constructor <- elements Common [Meta]
    constructor <$> genName

instance arbSimpleTerm :: Arbitrary SearchTermSimple where
  arbitrary = do
    len <- chooseInt 0 10
    ls <- vectorOf len arbitrary
    p <- arbitrary
    return $ SearchTermSimple ls p


instance arbSearchTerm :: Arbitrary SearchTerm where
  arbitrary = do
    constructor <- elements IncludeTerm [ExcludeTerm]
    constructor <$> arbitrary


instance arbSearchQuery :: Arbitrary SearchQuery where
  arbitrary = do
    depth <- chooseInt 0 10
    mkArbSearchQuery depth
    where mkArbSearchQuery n =
            case n of
              0 -> return EmptyQuery
              n -> do
                term <- arbitrary
                query <- mkArbSearchQuery (n -1)
                return $ SearchAnd term query

