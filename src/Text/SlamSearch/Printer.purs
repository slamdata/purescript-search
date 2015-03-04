module Text.SlamSearch.Printer (prettyQuery) where

import Data.Array (reverse)

import Text.SlamSearch.Parser
import Text.SlamSearch.Parser.Terms
import Text.SlamSearch.Parser.Values


prettyQuery :: SearchQuery -> String
prettyQuery EmptyQuery = ""
prettyQuery (SearchAnd term EmptyQuery) = prettyTerm term
prettyQuery (SearchAnd term query) = prettyTerm term <> " " <> prettyQuery query

prettyTerm :: SearchTerm -> String
prettyTerm (IncludeTerm term) = "+" <> prettySimpleTerm term
prettyTerm (ExcludeTerm term) = "-" <> prettySimpleTerm term


prettySimpleTerm :: SearchTermSimple -> String
prettySimpleTerm (SearchTermSimple ls p) = prettyLabels ls <> prettyPredicate p


prettyLabels :: [Label] -> String
prettyLabels ls =
  prettyLabels' (reverse ls) ""
  where prettyLabels' ls acc =
          case ls of
            [] -> acc
            (Common l):lls -> prettyLabels' lls (l <> ":" <> acc)
            (Meta l):lls -> prettyLabels' lls ("@" <> l <> ":" <> acc)

prettyPredicate :: Predicate -> String
prettyPredicate p =
  case p of
    ContainsPredicate v -> prettyValue v
    EqPredicate v -> "=" <> prettyValue v
    GtPredicate v -> ">" <> prettyValue v
    GtePredicate v -> ">=" <> prettyValue v
    LtePredicate v -> "<=" <> prettyValue v
    LtPredicate v -> "<" <> prettyValue v
    NePredicate v -> "<>" <> prettyValue v
    LikePredicate v -> "~" <> prettyValue v

  
prettyValue :: Value -> String
prettyValue value =
  case value of
    TextVal str -> str
    RangeVal str str' -> str <> ".." <> str'
    Tag str -> "#" <> str
    Glob str -> str 
