module Text.SlamSearch.Parser where

import Control.Apply
import Control.Alt
import Control.Alternative

import qualified Text.SlamSearch.Types as S
import qualified Text.SlamSearch.Parser.Tokens as Tk

import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Token as P


text :: P.Parser [Tk.Token] String
text = do
  txt <- P.when Tk.isText
  case txt of
    Tk.Text s -> return s
    _ -> P.fail "not text"

label :: P.Parser [Tk.Token] S.Label
label = do
  txt <- P.try do
    txt <- text
    P.match Tk.Colon
    pure txt
  pure $ S.Common txt

meta :: P.Parser [Tk.Token] S.Label
meta = do
  P.match Tk.At
  l <- label
  case l of
    S.Common t -> pure $ S.Meta t
    _ -> P.fail "incorrect metalabel"

slabel :: P.Parser [Tk.Token] S.Label
slabel = P.choice [P.try meta, label]
  

tag :: P.Parser [Tk.Token] S.Value
tag = do
  P.match Tk.Hash
  txt <- text
  pure $ S.Tag txt

range :: P.Parser [Tk.Token] S.Value
range = do
  bottom <- text
  P.match Tk.Range
  up <- text
  pure $ S.Range bottom up

val :: P.Parser [Tk.Token] S.Value
val = S.Text <$> text

svalue :: P.Parser [Tk.Token] S.Value
svalue = P.choice [P.try tag, P.try range, val]

  
type PredicateParser = P.Parser [Tk.Token] S.Predicate

contains :: PredicateParser
contains = S.Contains <$> svalue

eq :: PredicateParser
eq = P.match Tk.Eq *> (S.Eq <$> svalue)

gt :: PredicateParser
gt = P.match Tk.Gt *> (S.Gt <$> svalue)

gte :: PredicateParser
gte = P.match Tk.GtE *> (S.Gte <$> svalue)

lt :: PredicateParser
lt = P.match Tk.Lt *> (S.Lt <$> svalue)

lte :: PredicateParser
lte = P.match Tk.LtE *> (S.Lte <$> svalue)

ne :: PredicateParser
ne = P.match Tk.Ne *> (S.Ne <$> svalue)

like :: PredicateParser
like = P.match Tk.Tilde *> (S.Like <$> text)

predicate :: PredicateParser
predicate = P.choice [P.try like,
                      P.try ne,
                      P.try lte,
                      P.try lt,
                      P.try gt,
                      P.try eq,
                      contains]

term :: P.Parser [Tk.Token] S.Term
term = do
  included <- P.option true do
    P.try (P.match Tk.Minus *> (pure false))
    <|>
    P.try (P.match Tk.Plus *> (pure true))

  labels <- many slabel
  predicate <- predicate

  pure {include: included,
        labels: labels,
        predicate: predicate}
