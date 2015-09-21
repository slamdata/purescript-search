module Text.SlamSearch.Printer
  ( strLabel
  , strValue
  , strTerm
  , strQuery
  ) where

import Prelude
import Data.Foldable (fold, foldr, Foldable)
import Data.Array (reverse, intersect, length)
import Data.String (trim, split, toCharArray)
import Text.SlamSearch.Types
import Text.SlamSearch.Parser.Tokens (keyChars)
import Data.Semiring.Free (runFree)

strLabel :: Label -> String
strLabel l = case l of
  Common str -> str <> ":"
  Meta str -> "@" <> str <> ":"

strValue :: Value -> String
strValue v = case v of
  Text str -> quote str
  Range bot up -> bot <> ".." <> up
  Tag str -> "#" <> str
  where
    quote str =
      if length ((toCharArray str) `intersect` keyChars) > 0
        then "\"" <> str <> "\""
        else str

strPredicate :: Predicate -> String
strPredicate pr = case pr of
  Contains v -> strValue v
  Eq v -> "=" <> strValue v
  Gt v -> ">" <> strValue v
  Gte v -> ">=" <> strValue v
  Lt v -> "<" <> strValue v
  Lte v -> "<=" <> strValue v
  Ne v -> "<>" <> strValue v
  Like str -> "~" <> str

strTerm :: Term -> String
strTerm (Term {include: include, labels: labels, predicate: predicate}) =
  strInclude include <> strLabels labels <> strPredicate predicate
  where
    strInclude :: Boolean -> String
    strInclude true = "+"
    strInclude _ = "-"

    strLabels :: forall f. (Foldable f, Functor f) => f Label -> String
    strLabels ls = fold $ strLabel <$> ls


strQuery :: SearchQuery -> String
strQuery query =
  let terms = runFree query in
  trim
    $ foldr (\a b -> b <> " " <> a) ""
    $ foldr (\a b -> b <> " " <> strTerm a) ""
   <$> terms

