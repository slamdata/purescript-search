module Test.Check.Gen where

import Prelude
import Test.StrongCheck.Gen
import Test.StrongCheck hiding (test)

import Data.String
import Data.Char
import Data.Semiring.Free 
import Data.List (toList, List(..))

import Text.SlamSearch.Types
import Data.Foldable (foldl)

validChars :: String
validChars = "bcdefghijklmnopqrstuvwxyz" <>
             "ABCDEFGHIJKLMNOPQRSTUVWXYZ" <>
             "01234567890"

genGenName :: String -> Gen String
genGenName strin = do
  len <- chooseInt 1.0 5.0
  genName' len ""
  where genName' len acc =
          case len of
            0 -> return acc
            n -> do
              ch <- fromChar <$> elements (fromCharCode 65)
                    (toList $ toCharArray validChars)
                    
              genName' (len - 1) (ch <> acc)
              
genName :: Gen String
genName = genGenName validChars 

instance arbitraryValue :: Arbitrary Value where
  arbitrary = do
    str <- genName
    str' <- genName
    elements (Text str) $ toList [
      Range str str',
      Tag str
      ]

instance arbLabel :: Arbitrary Label where
  arbitrary = do
    constructor <- elements Common (Cons Meta Nil)
    constructor <$> genName

instance arbPredicate :: Arbitrary Predicate where
  arbitrary = do
    val <- arbitrary
    str <- genName 
    elements (Contains val) $ toList [
      Eq val, Gt val, Gte val, Lt val, Lte val, Ne val, Like str
      ]

instance arbTerm :: Arbitrary Term where
  arbitrary = do
    r <- {include: _, labels: _, predicate: _}
         <$> arbitrary
         <*> (toList <$> (arbitrary :: Gen (Array Label)))
         <*> arbitrary
    pure $ Term r 

-- helper type not to add orphan instances of Arbitrary on Free a 
newtype QueryWrapper = QueryWrapper SearchQuery 

instance arbQueryWrapper :: Arbitrary QueryWrapper where
  arbitrary = do
    k <- chooseInt 1.0 10.0
    lst <- vectorOf k $ free <$> arbitrary
    pure <<< QueryWrapper $ foldl (*) one lst

