module Text.SlamSearch.Parser.Tokens (
  tokens,
  Token(..),
  isText,
  keyChars
  ) where

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators 
import Text.Parsing.Parser.String

import Control.Apply
import Control.Alt
import Control.Alternative 
import Data.Foldable
import Data.Array (length)
import Data.Either

keyChars = [
  ",",
  ".",
  "~",
  "!",
  "@",
  "#",
  "$",
  "^",
  "&",
  "*",
  "?",
  "(",
  ")",
  "-",
  "+",
  "=",
  "<",
  ">",
  "[",
  "]",
  " ",
  "\t",
  "\r",
  "\n",
  "\"",
  ":",
  ""
  ]

rawString :: Parser String String
rawString = do
  cs <- many $ noneOf keyChars
  case cs of
    [] -> fail "incorrect raw string"
    cs -> return (fold cs)
    

slashed :: Parser String String
slashed = do
  slash <- string "\\"
  ch <- char
  return $ slash <> ch

quotedSymbol :: Parser String String
quotedSymbol = do
  (try slashed) <|> noneOf ["\""]

quotedString :: Parser String String
quotedString = do
  between (string "\"") (string "\"") $ do
    cs <- many quotedSymbol
    case cs of
      [] -> fail "incorrect quoted string"
      cs -> return $ "\"" <> (fold cs) <> "\""

data Token =
  Text String
  | Star
  | Range
  | QMark
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
    Star -> "Star"
    Range -> "Range"
    QMark -> "QMark"
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
  (==) (Text s) (Text s') = s == s'
  (==) Star Star = true
  (==) Range Range = true
  (==) QMark QMark = true
  (==) Hash Hash = true
  (==) Plus Plus = true
  (==) Minus Minus = true
  (==) At At = true
  (==) Eq Eq = true
  (==) Lt Lt = true
  (==) Gt Gt = true
  (==) LtE LtE = true
  (==) GtE GtE = true
  (==) Ne Ne = true
  (==) Tilde Tilde = true
  (==) Colon Colon = true
  (==) _ _ = false
  (/=) a b = not $ a == b

isText :: Token -> Boolean
isText (Text _) = true
isText _ = false

raw :: Parser String Token
raw = Text <$> rawString 

quoted :: Parser String Token
quoted = Text <$> quotedString 

star :: Parser String Token
star = pure Star <* string "*" 

range :: Parser String Token
range = pure Range <* string ".." 

qmark :: Parser String Token
qmark = pure QMark <* string "?"

hash :: Parser String Token
hash = pure Hash <* string "#"

plus :: Parser String Token
plus = pure Plus <* string "+"

minus :: Parser String Token
minus = pure Minus <* string "-"

at :: Parser String Token
at = pure At <* string "@" 

eq :: Parser String Token
eq = pure Eq <* string "=" 

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


tokenize :: Parser String [Token]
tokenize = many $ choice [colon, tilde, ne, gte, lte, gt,
                          lt, eq, at, minus, plus, hash,
                          qmark, range, star, quoted, raw]




tokens :: String -> Either ParseError [Token]
tokens input = runParser input tokenize

