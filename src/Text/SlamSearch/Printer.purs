module Text.SlamSearch.Printer
  ( strLabel
  , strValue
  , strTerm
  , strQuery
  ) where

import Prelude
import Data.Foldable (foldr, Foldable, foldMap)
import Data.Array (intersect, length)
import Data.List (List())
import Data.String (trim, toCharArray)
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
  Range v vv -> strValue v <> ".." <> strValue vv

strTerm :: Term -> String
strTerm (Term {include: include, labels: labels, predicate: predicate}) =
  strInclude include <> strLabels labels <> strPredicate predicate
  where
  strInclude :: Boolean -> String
  strInclude true = "+"
  strInclude _ = "-"

  strLabels :: forall f. (Foldable f) => f Label -> String
  strLabels ls = foldMap strLabel ls

strQuery :: SearchQuery -> String
strQuery query =
  trim
  $ foldr (\a b -> b <> " " <> a) ""
  $ foldr (\a b -> b <> " " <> strTerm a) ""
  <$> terms
  where
  terms :: List (List Term)
  terms = runFree query
