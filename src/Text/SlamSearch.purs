module Text.SlamSearch where

import Data.Semiring.Free
import Data.String
import Data.Monoid
import Data.Foldable
import Data.Either
import Text.SlamSearch.Types
import qualified Text.SlamSearch.Parser.Tokens as Tk
import qualified Text.SlamSearch.Parser as S
import qualified Text.Parsing.Parser as P

mkQuery :: String -> SearchQuery
mkQuery input =
  liftFree mkTerm $ prepare input
  where prepare :: String -> Free String
        prepare input = foldl (*) one $ free <$> split " " input

        mkTerm :: String -> SearchQuery
        mkTerm input =
          either (const one) free $ parseTerm input

        parseTerm :: String -> Either P.ParseError Term
        parseTerm input = Tk.tokens input >>= flip P.runParser S.term

instance semiringBool :: Semiring Boolean where
  one = true
  zero = false
  (*) = (&&)
  (+) = (||)

check :: forall a. a -> SearchQuery -> (a -> Term -> Boolean) -> Boolean 
check input query checkOneTerm = liftFree (checkOneTerm input) $ query
