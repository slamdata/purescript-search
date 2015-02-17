module Text.SlamSearch.Parser.Values (
  values, Value(..),
  isTextual, isLabel, isMeta
  ) where

import Control.Apply
import Control.Alt
import Control.Alternative
import Data.Foldable
import Data.Array (length)
import Data.Either

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Token

import Text.SlamSearch.Parser.Tokens

data Value =
  TextVal String
  | RangeVal String String
  | Tag String
  | Label String
  | MetaLabel String
  | Glob String
  | Through Token

isTextVal :: Value -> Boolean
isTextVal (TextVal _) = true
isTextVal _ = false

isRangeVal :: Value -> Boolean
isRangeVal (RangeVal _ _) = true 
isRangeVal _ = false

isTag :: Value -> Boolean
isTag (Tag _) = true
isTag _ = false

isLabel :: Value -> Boolean
isLabel (Label _) = true
isLabel _ = false

isMeta :: Value -> Boolean
isMeta (MetaLabel _) = true
isMeta _ = false

isGlob :: Value -> Boolean
isGlob (Glob _) = true
isGlob _ = false 

isTextual :: Value -> Boolean
isTextual v = isGlob v || isTextVal v || isRangeVal v || isTag v

instance valueShow :: Show Value where
  show val = case val of
    TextVal str -> "TextVal(" <> str <> ")"
    RangeVal str str' -> "Range(" <> str <> ".." <> str' <> ")"
    Tag str -> "Tag(" <> str <> ")"
    Label str -> "Label(" <> str <> ")"
    MetaLabel str -> "MetaLabel(" <> str <> ")"
    Glob str -> "Glob(" <> str <> ")"
    Through tok -> "Through(" <> show tok <> ")"

instance valueEq :: Eq Value where
  (==) (TextVal v) (TextVal v') = v == v'
  (==) (RangeVal v v') (RangeVal vv vv') = v == vv && v' == vv'
  (==) (Tag v) (Tag v') = v == v'
  (==) (Label v) (Label v') = v == v'
  (==) (MetaLabel v) (MetaLabel v') = v == v'
  (==) (Glob v) (Glob v') = v == v'
  (==) (Through t) (Through t') = t == t'
  (==) _ _ = false
  (/=) a b = not $ a == b

text :: Parser [Token] String
text = do
  txt <- when isText
  case txt of
    Text s -> return s
    _ -> fail "not text"

tag :: Parser [Token] Value
tag = match Hash *> when isText >>= \(Text txt) -> return $ Tag txt

label :: Parser [Token] Value
label = do
  txt <- try do
    txt <- text
    match Colon
    return txt
  return $ Label txt

meta :: Parser [Token] Value
meta = do
  match At
  l <- label
  case l of
    Label t -> return $ MetaLabel t
    _ -> fail "incorrect metalabel"

rangeVal :: Parser [Token] Value
rangeVal = do
  bottom <- text
  match Range
  up <- text

  return $ RangeVal bottom up

globSymb :: Parser [Token] String
globSymb = (match Star *> return "*")
           <|> (match QMark *> return "?")

globTextP :: Parser [Token] String
globTextP = try do
  ss <- fold <$> (try $ many globSymb)
  txt <- try text
  case ss  of
    "" -> fail "incorrect glob text"
    res -> return $ ss <> txt
    
globTextA :: Parser [Token] String
globTextA = try do
  txt <- try text
  s <- lookAhead globSymb
  return $ txt 



glob :: Parser [Token] Value
glob = do
  strs <- many $
          globTextP <|> globTextA <|> globSymb

  case strs of
    [] -> fail "incorrect glob"
    _ -> return $ Glob (fold strs)

simple :: Parser [Token] Value
simple = try do
  txt <- text
  return $ TextVal txt
  
  
through :: Parser [Token] Value
through = Through <$> token

vals :: Parser [Token] [Value]
vals = many $ do
  label 
    <|> try rangeVal
    <|> try meta
    <|> try glob
    <|> simple
    <|> tag
    <|> through

values :: [Token] -> Either ParseError [Value]
values input = runParser input vals
