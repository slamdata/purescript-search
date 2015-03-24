module Text.SlamSearch.Printer where

import Data.Foldable (fold, foldr)
import Data.Array (reverse)
import Data.String (trim)
import Text.SlamSearch.Types
import Data.Semiring.Free

printLabel :: Label -> String
printLabel l = case l of
  Common str -> str <> ":"
  Meta str -> "@" <> str <> ":"

printValue :: Value -> String
printValue v = case v of
  Text str -> str
  Range bot up -> bot <> ".." <> up
  Tag str -> "#" <> str

printPredicate :: Predicate -> String
printPredicate pr = case pr of
  Contains v -> printValue v
  Eq v -> "=" <> printValue v
  Gt v -> ">" <> printValue v
  Gte v -> ">=" <> printValue v
  Lt v -> "<" <> printValue v
  Lte v -> "<=" <> printValue v
  Ne v -> "<>" <> printValue v
  Like str -> "~" <> str

printTerm :: Term -> String
printTerm {include: include, labels: labels, predicate: predicate} =
  printInclude include <> printLabels labels <> printPredicate predicate
  where printInclude :: Boolean -> String
        printInclude true = "+"
        printInclude _ = "-"

        printLabels :: [Label] -> String
        printLabels ls = fold $ printLabel <$> ls


printQuery :: SearchQuery -> String
printQuery query =
  let terms = runFree query in
  trim $ 
  foldr (\a b -> b <> " " <> a) "" $
  foldr (\a b -> b <> " " <> printTerm a) "" <$>
  terms 
        
