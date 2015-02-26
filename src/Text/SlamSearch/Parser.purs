module Text.SlamSearch.Parser where

import Data.Either
import Text.Parsing.Parser
import Text.SlamSearch.Parser.Terms
import Text.SlamSearch.Parser.Tokens
import Text.SlamSearch.Parser.Values

parseSearchQuery :: String -> Either ParseError [SearchTerm]
parseSearchQuery input =
  pure input >>=
  tokens >>=
  values >>=
  search



