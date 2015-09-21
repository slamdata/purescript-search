module Text.SlamSearch.Parser.Tokens
  ( tokens
  , Token(..)
  , isText
  , keyChars
  ) where

import Prelude

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String

import Control.Apply ((<*))
import Control.Alt ((<|>))
import Data.Foldable (fold)
import Data.List (many, List(..), fromList)
import Data.Either (Either(..))
import Data.String (fromChar, toCharArray, fromCharArray)
import Data.Char (fromCharCode)

keyChars :: Array Char
keyChars = toCharArray $ fold
  [ ","
  , "."
  , "~"
  , "!"
  , "@"
  , "#"
  , "$"
  , "^"
  , "&"
  , "("
  , ")"
  , "-"
  , "+"
  , "="
  , "<"
  , ">"
  , "["
  , "]"
  , " "
  , "\t"
  , "\r"
  , "\n"
  , "\""
  , ":"
  , ""
  ]

rawString :: Parser String String
rawString = do
  cs <- many $ noneOf keyChars
  case cs of
    Nil -> fail "incorrect raw string"
    cs -> pure $ fromCharArray $ fromList cs


slashed :: Parser String String
slashed = do
  slash <- string "\\"
  ch <- anyChar
  return $ slash <> fromChar ch

quotedSymbol :: Parser String String
quotedSymbol = do
  (try slashed) <|> (fromChar <$> noneOf [fromCharCode 34])

quotedString :: Parser String String
quotedString = do
  between (string "\"") (string "\"") $ do
    cs <- many quotedSymbol
    case cs of
      Nil -> fail "incorrect quoted string"
      cs -> pure $ fold cs

data Token
  = Text String
  | Range
  | Hash
  | Plus
  | Minus
  | At
  | Eq
  | Lt
  | Gt
  | LtE
  | GtE
  | Ne
  | Tilde
  | Colon

instance showToken :: Show Token where
  show t = case t of
    Text s -> "Text(" <> s <> ")"
    Range -> "Range"
    Hash -> "Hash"
    Plus -> "Plus"
    Minus -> "Minus"
    At -> "At"
    Eq -> "Eq"
    Lt -> "Lt"
    Gt -> "Gt"
    LtE -> "LtE"
    GtE -> "GtE"
    Ne -> "Ne"
    Tilde -> "Tilde"
    Colon -> "Colon"

instance eqToken :: Eq Token where
  eq (Text s) (Text s') = s == s'
  eq Range Range = true
  eq Hash Hash = true
  eq Plus Plus = true
  eq Minus Minus = true
  eq At At = true
  eq Eq Eq = true
  eq Lt Lt = true
  eq Gt Gt = true
  eq LtE LtE = true
  eq GtE GtE = true
  eq Ne Ne = true
  eq Tilde Tilde = true
  eq Colon Colon = true
  eq _ _ = false

isText :: Token -> Boolean
isText (Text _) = true
isText _ = false

raw :: Parser String Token
raw = Text <$> rawString

quoted :: Parser String Token
quoted = Text <$> quotedString

range :: Parser String Token
range = pure Range <* string ".."

hash :: Parser String Token
hash = pure Hash <* string "#"

plus :: Parser String Token
plus = pure Plus <* string "+"

minus :: Parser String Token
minus = pure Minus <* string "-"

at :: Parser String Token
at = pure At <* string "@"

eq_ :: Parser String Token
eq_ = pure Eq <* string "="

lt :: Parser String Token
lt = pure Lt <* string "<"

gt :: Parser String Token
gt = pure Gt <* string ">"

lte :: Parser String Token
lte = pure LtE <* string "<="

gte :: Parser String Token
gte = pure GtE <* string ">="

ne :: Parser String Token
ne = pure Ne <* (string "!=" <|> string "<>")

tilde :: Parser String Token
tilde = pure Tilde <* string "~"

colon :: Parser String Token
colon = pure Colon <* string ":"

tokenize :: Parser String (List Token)
tokenize = many $ choice
  [ colon, tilde, ne, gte, lte, gt
  , lt, eq_, at, minus, plus, hash
  , range, quoted, raw
  ]

tokens :: String -> Either ParseError (List Token)
tokens input = runParser input tokenize

