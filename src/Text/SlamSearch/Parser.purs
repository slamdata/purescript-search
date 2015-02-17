module Text.SlamSearch.Parser (
  parseSearchQuery, parseSearchTerm, SearchQuery(..)
  ) where

import Data.Either
import Data.Traversable
import Data.String
import Data.Array (filter, reverse)

import Text.Parsing.Parser 
import Text.SlamSearch.Parser.Terms (search, SearchTerm(..))
import Text.SlamSearch.Parser.Tokens (tokens)
import Text.SlamSearch.Parser.Values (values)

data SearchQuery =
  EmptyQuery
  | SearchAnd SearchTerm SearchQuery

instance showSearchQuery :: Show SearchQuery where
  show EmptyQuery = "EmptyQuery"
  show (SearchAnd term query) = "SearchAnd(" <> show term <> "," <> show query <> ")"

instance eqSearchQuery :: Eq SearchQuery where
  (==) EmptyQuery EmptyQuery = true
  (==) (SearchAnd t q) (SearchAnd t' q') =
    (t == t') && (q == q')
  (==) _ _ = false
  (/=) a b = not $ a == b

termsToQuery :: [SearchTerm] -> SearchQuery
termsToQuery terms =
  termsToQuery' terms EmptyQuery
  where termsToQuery' terms query =
          case terms of
            [] -> query
            term:ts -> termsToQuery' ts (SearchAnd term query)

parseSearchQuery :: String -> Either ParseError SearchQuery
parseSearchQuery input = termsToQuery <$> parseManyTerms input

parseManyTerms :: String -> Either ParseError [SearchTerm]
parseManyTerms input =
  let words = reverse $ filter ((/=) "") $ split " " input in
  traverse parseSearchTerm words
  

parseSearchTerm :: String -> Either ParseError SearchTerm
parseSearchTerm input =
  pure input >>=
  tokens >>=
  values >>=
  search



