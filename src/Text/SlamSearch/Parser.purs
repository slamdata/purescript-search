module Text.SlamSearch.Parser (term) where

import Prelude

import Control.Apply ((*>))
import Control.Alt ((<|>))
import Data.List (many, List(), null)

import Text.SlamSearch.Types as S
import Text.SlamSearch.Parser.Tokens as Tk

import Text.Parsing.Parser.Pos (initialPos, Position())
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.Token as P

notCarePos :: forall a. a -> Position
notCarePos = const initialPos

text :: P.Parser (List Tk.Token) String
text = do
  txt <- P.when notCarePos Tk.isText
  case txt of
    Tk.Text s -> return s
    _ -> P.fail "not text"

label :: P.Parser (List Tk.Token) S.Label
label = do
  txt <- P.try do
    txt <- text
    P.match notCarePos Tk.Colon
    pure txt
  pure $ S.Common txt

meta :: P.Parser (List Tk.Token) S.Label
meta = do
  P.match notCarePos Tk.At
  l <- label
  case l of
    S.Common t -> pure $ S.Meta t
    _ -> P.fail "incorrect metalabel"

slabel :: P.Parser (List Tk.Token) S.Label
slabel = P.choice [P.try meta, label]

tag :: P.Parser (List Tk.Token) S.Value
tag = do
  P.match notCarePos Tk.Hash
  txt <- text
  pure $ S.Tag txt

val :: P.Parser (List Tk.Token) S.Value
val = S.Text <$> text

svalue :: P.Parser (List Tk.Token) S.Value
svalue = P.choice [P.try tag, val]

type PredicateParser = P.Parser (List Tk.Token) S.Predicate

contains :: PredicateParser
contains = S.Contains <$> svalue

eq_ :: PredicateParser
eq_ = P.match notCarePos Tk.Eq *> (S.Eq <$> svalue)

gt :: PredicateParser
gt = P.match notCarePos Tk.Gt *> (S.Gt <$> svalue)

gte :: PredicateParser
gte = P.match notCarePos Tk.GtE *> (S.Gte <$> svalue)

lt :: PredicateParser
lt = P.match notCarePos Tk.Lt *> (S.Lt <$> svalue)

lte :: PredicateParser
lte = P.match notCarePos Tk.LtE *> (S.Lte <$> svalue)

ne :: PredicateParser
ne = P.match notCarePos Tk.Ne *> (S.Ne <$> svalue)

like :: PredicateParser
like = P.match notCarePos Tk.Tilde *> (S.Like <$> text)

range :: PredicateParser
range = do
  bottom <- svalue
  P.match notCarePos Tk.Range
  up <- svalue
  pure $ S.Range bottom up

predicate :: PredicateParser
predicate = P.choice [ P.try like
                     , P.try ne
                     , P.try lte
                     , P.try lt
                     , P.try gt
                     , P.try gte
                     , P.try eq_
                     , P.try range
                     , contains
                     ]
-- | Parse list of `Token`s to `Term`
term :: P.Parser (List Tk.Token) S.Term
term = do
  included <- P.option true
              $   (P.match notCarePos Tk.Plus *> pure true)
              <|> (P.match notCarePos Tk.Minus *> pure false)
  labels <- many slabel P.<?> "label"
  pred <- predicate P.<?> "predicate"
  rest <- many $ P.try $ P.token notCarePos
  if not $ null rest
    then P.fail "incorrect query string"
    else
    pure $ S.Term $ { include: included
                    , labels: labels
                    , predicate: pred
                    }
