module Text.SlamSearch.Printer
  ( strLabel
  , strValue
  , strTerm
  , strQuery
  ) where

import Prelude

import Data.Foldable (class Foldable, foldr, foldMap)
import Data.Array (intersect, length)
import Data.String (trim, toCharArray)
import Text.SlamSearch.Types as SS
import Text.SlamSearch.Parser.Tokens (keyChars)
import Data.Semiring.Free (runFree)

strLabel ∷ SS.Label → String
strLabel =
  case _ of
    SS.Common str → str <> ":"
    SS.Meta str → "@" <> str <> ":"

strValue ∷ SS.Value → String
strValue =
  case _ of
    SS.Text str → quote str
    SS.Tag str → "#" <> str
  where
  quote str =
    if length ((toCharArray str) `intersect` keyChars) > 0
    then "\"" <> str <> "\""
    else str

strPredicate ∷ SS.Predicate → String
strPredicate =
  case _ of
    SS.Contains v → strValue v
    SS.Eq v → "=" <> strValue v
    SS.Gt v → ">" <> strValue v
    SS.Gte v → ">=" <> strValue v
    SS.Lt v → "<" <> strValue v
    SS.Lte v → "<=" <> strValue v
    SS.Ne v → "<>" <> strValue v
    SS.Like str → "~" <> str
    SS.Range v vv → strValue v <> ".." <> strValue vv

strTerm ∷ SS.Term → String
strTerm (SS.Term { include, labels, predicate }) =
  strInclude include <> strLabels labels <> strPredicate predicate
  where
  strInclude ∷ Boolean → String
  strInclude true = "+"
  strInclude _ = "-"

  strLabels ∷ ∀ f. (Foldable f) ⇒ f SS.Label → String
  strLabels ls = foldMap strLabel ls

strQuery ∷ SS.SearchQuery → String
strQuery =
  trim
    <<< foldr (\a b → b <> " " <> a) ""
    <<< map (foldr (\a b → b <> " " <> strTerm a) "")
    <<< runFree
