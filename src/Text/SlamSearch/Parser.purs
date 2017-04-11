module Text.SlamSearch.Parser (term) where

import Prelude

import Control.Alt ((<|>))
import Data.List (List, many, null)

import Text.SlamSearch.Types as S
import Text.SlamSearch.Parser.Tokens as Tk

import Text.Parsing.Parser.Pos (Position, initialPos)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.Token as PT

notCarePos ∷ ∀ a. a → Position
notCarePos = const initialPos

text ∷ P.Parser (List Tk.Token) String
text = do
  txt ← PT.when notCarePos Tk.isText
  case txt of
    Tk.Text s → pure s
    _ → P.fail "not text"

label ∷ P.Parser (List Tk.Token) S.Label
label = do
  txt ← PC.try do
    txt' ← text
    _ ← PT.match notCarePos Tk.Colon
    pure txt'
  pure $ S.Common txt

meta ∷ P.Parser (List Tk.Token) S.Label
meta = do
  _ ← PT.match notCarePos Tk.At
  l ← label
  case l of
    S.Common t → pure $ S.Meta t
    _ → P.fail "incorrect metalabel"

slabel ∷ P.Parser (List Tk.Token) S.Label
slabel = PC.choice [ PC.try meta, label ]

tag ∷ P.Parser (List Tk.Token) S.Value
tag = do
  _ ← PT.match notCarePos Tk.Hash
  txt ← text
  pure $ S.Tag txt

val ∷ P.Parser (List Tk.Token) S.Value
val = S.Text <$> text

svalue ∷ P.Parser (List Tk.Token) S.Value
svalue = PC.choice [ PC.try tag, val ]

type PredicateParser = P.Parser (List Tk.Token) S.Predicate

contains ∷ PredicateParser
contains = S.Contains <$> svalue

eq_ ∷ PredicateParser
eq_ = PT.match notCarePos Tk.Eq *> (S.Eq <$> svalue)

gt ∷ PredicateParser
gt = PT.match notCarePos Tk.Gt *> (S.Gt <$> svalue)

gte ∷ PredicateParser
gte = PT.match notCarePos Tk.GtE *> (S.Gte <$> svalue)

lt ∷ PredicateParser
lt = PT.match notCarePos Tk.Lt *> (S.Lt <$> svalue)

lte ∷ PredicateParser
lte = PT.match notCarePos Tk.LtE *> (S.Lte <$> svalue)

ne ∷ PredicateParser
ne = PT.match notCarePos Tk.Ne *> (S.Ne <$> svalue)

like ∷ PredicateParser
like = PT.match notCarePos Tk.Tilde *> (S.Like <$> text)

range ∷ PredicateParser
range = do
  bottom ← svalue
  _ ← PT.match notCarePos Tk.Range
  up ← svalue
  pure $ S.Range bottom up

predicate ∷ PredicateParser
predicate =
  PC.choice
    [ PC.try like
    , PC.try ne
    , PC.try lte
    , PC.try lt
    , PC.try gt
    , PC.try gte
    , PC.try eq_
    , PC.try range
    , contains
    ]

-- | Parse list of `Token`s to `Term`
term ∷ P.Parser (List Tk.Token) S.Term
term = do
  included ←
    PC.option true $
      (PT.match notCarePos Tk.Plus *> pure true)
      <|> (PT.match notCarePos Tk.Minus *> pure false)
  labels ← many slabel PC.<?> "label"
  pred ← predicate PC.<?> "predicate"
  rest ← many $ PC.try $ PT.token notCarePos
  if not $ null rest
    then P.fail "incorrect query string"
    else pure $ S.Term $ { include: included, labels: labels, predicate: pred }
