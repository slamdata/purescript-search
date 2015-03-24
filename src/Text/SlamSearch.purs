module Text.SlamSearch where


import Data.Semiring.Free
import Data.String
import Data.Foldable
import Data.Either
import Data.Array (reverse)
import qualified Data.Semiring.Disjunctive as Dj

import Text.SlamSearch.Types
import qualified Text.SlamSearch.Parser.Tokens as Tk
import qualified Text.SlamSearch.Parser as S
import qualified Text.Parsing.Parser as P


mkQuery :: String -> SearchQuery
mkQuery input =
  liftFree mkTerm $ prepare input
  where prepare :: String -> Free String
        prepare input = foldl (*) one $ free <$> reverse (split " " input)

        
        mkTerm :: String -> SearchQuery
        mkTerm input =
          either (const one) free $ parseTerm input

        parseTerm :: String -> Either P.ParseError Term
        parseTerm input = Tk.tokens input >>= flip P.runParser S.term



check :: forall a. a -> SearchQuery -> (a -> Term -> Boolean) -> Boolean 
check input query checkOneTerm =
  Dj.runDisjunctive $
  liftFree (Dj.Disjunctive <<< checkOneTerm input) $ query
