module Text.SlamSearch
  ( mkQuery
  , check
  ) where

import Prelude
import Data.Semiring.Free (Free, liftFree, free)
import Data.Foldable (fold, foldl)
import Data.Traversable (sequence)
import Data.Either (Either)

import Data.String (split, trim)
import Data.List (List(..), reverse, fromFoldable)
import Data.Monoid.Disj as Dj

import Text.SlamSearch.Types (Term, SearchQuery)
import Text.SlamSearch.Parser.Tokens as Tk
import Text.SlamSearch.Parser as S
import Text.Parsing.Parser as P

-- | Split input string to list of string by spaces if those spaces
-- | aren't located between `"` (They are effectively parsed as `AND`/`*`)
-- | `"foo bar"` -→ `Cons "foo" $ Cons "bar" Nil`
-- | `"fo\"o b\"ar"` -→ `Cons "fo\"o b\"ar" Nil`
splitBySpaces ∷ String → List String
splitBySpaces input =
  fold <<< reverse <$> splitBySpaces' (fromFoldable $ split "" input) (Cons Nil Nil) false

  where
  splitBySpaces' ∷ List String → List (List String) → Boolean → List (List String)
  splitBySpaces' (Cons char cs) (Cons current accum) quoted =
    let newWord = Cons char current
        newAccum = Cons newWord accum
    in case char of
      "\"" → splitBySpaces' cs newAccum (not quoted)
      " " → if not quoted
             then splitBySpaces' cs (Cons Nil (Cons current accum)) false
             else splitBySpaces' cs newAccum true
      _ → splitBySpaces' cs newAccum quoted
  splitBySpaces' _ acc _ = acc

-- | Parse string to `SearchQuery`.
mkQuery
  ∷ String
  → Either P.ParseError SearchQuery
mkQuery =
  sequence <<< mkTerms <<< trim

  where
  prepare ∷ String → Free String
  prepare input = foldl (*) one $ free <$> splitBySpaces input

  mkTerms ∷ String → Free (Either P.ParseError Term)
  mkTerms input = map parseTerm $ prepare input

  parseTerm ∷ String → Either P.ParseError Term
  parseTerm input = Tk.tokens input >>= flip P.runParser S.term

-- | Having predicate on `Term` check if `SearchQuery` satisfies it.
check
  ∷ ∀ a
  . a
  → SearchQuery
  → (a → Term → Boolean)
  → Boolean
check input query checkOneTerm =
  Dj.runDisj $ liftFree (Dj.Disj <<< checkOneTerm input) query
