module Text.SlamSearch where


import Data.Semiring.Free
import Data.Foldable
import Data.Traversable
import Data.Either

import Data.String (joinWith, split, trim)
import Data.List
import qualified Data.Semiring.Disjunctive as Dj

import Text.SlamSearch.Types
import qualified Text.SlamSearch.Parser.Tokens as Tk
import qualified Text.SlamSearch.Parser as S
import qualified Text.Parsing.Parser as P
--import qualified Data.Array as A
--import Data.Array (reverse)



splitBySpaces :: String -> [String]
splitBySpaces input =
  toArray $
  (fold <<< reverse) <$>
  splitBySpaces' (fromArray $ split "" input) (Cons Nil Nil) false
  
  where splitBySpaces' input (Cons current accum) quoted =
          case input of
            Nil -> Cons current accum
            Cons char cs ->
              let newWord = Cons char current
                  newAccum = Cons newWord accum 
              in case char of
                "\"" -> splitBySpaces' cs newAccum (not quoted)
                " " -> 
                  if not quoted then
                    splitBySpaces' cs (Cons Nil (Cons current accum)) false 
                  else
                    splitBySpaces' cs newAccum true 
                _ -> splitBySpaces' cs newAccum quoted

mkQuery :: String -> Either P.ParseError SearchQuery
mkQuery input =
  sequence $ mkTerms (trim input)
  where prepare :: String -> Free String
        prepare input = foldl (*) one $ free <$> splitBySpaces input
        
        mkTerms :: String -> Free (Either P.ParseError Term) 
        mkTerms input =
          parseTerm <$> prepare input 

        parseTerm :: String -> Either P.ParseError Term
        parseTerm input = Tk.tokens input >>= flip P.runParser S.term

              
check :: forall a. a -> SearchQuery -> (a -> Term -> Boolean) -> Boolean 
check input query checkOneTerm =
  Dj.runDisjunctive $
  liftFree (Dj.Disjunctive <<< checkOneTerm input) $ query
