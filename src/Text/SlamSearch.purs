module Text.SlamSearch where


import Data.Semiring.Free
import Data.String
import Data.Foldable
import Data.Traversable
import Data.Either
import Data.Array (reverse)
import qualified Data.Semiring.Disjunctive as Dj

import Text.SlamSearch.Types
import qualified Text.SlamSearch.Parser.Tokens as Tk
import qualified Text.SlamSearch.Parser as S
import qualified Text.Parsing.Parser as P



mkQuery :: String -> Either P.ParseError SearchQuery
mkQuery input =
  sequenceFree $ mkTerms input 
  where prepare :: String -> Free String
        prepare input = foldl (*) one $ free <$> reverse (split " " input)
        
        mkTerms :: String -> Free (Either P.ParseError Term) 
        mkTerms input =
          parseTerm <$> prepare input 

        parseTerm :: String -> Either P.ParseError Term
        parseTerm input = Tk.tokens input >>= flip P.runParser S.term

              
check :: forall a. a -> SearchQuery -> (a -> Term -> Boolean) -> Boolean 
check input query checkOneTerm =
  Dj.runDisjunctive $
  liftFree (Dj.Disjunctive <<< checkOneTerm input) $ query
