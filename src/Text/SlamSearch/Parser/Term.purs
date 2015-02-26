module Text.SlamSearch.Parser.Terms where

import Text.SlamSearch.Parser.Values
import Text.SlamSearch.Parser.Tokens
import Text.SlamSearch.Parser.Utils

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Control.Apply
import Control.Alt
import Control.Alternative
import Data.Tuple
import Data.Either

data SimplePredicate =
  ContainsP Value
  | EqP Value
  | GtP Value
  | GteP Value
  | LteP Value
  | LtP Value
  | NeP Value
  | LikeP Value

instance showSimplePredicate :: Show SimplePredicate where
  show a = case a of
    ContainsP p -> "ContainsP(" <> show p <> ")"
    EqP p -> "EqP(" <> show p <> ")"
    GtP p -> "GtP(" <> show p <> ")"
    GteP p -> "GteP(" <> show p <> ")"
    LteP p -> "GteP(" <> show p <> ")"
    LtP p -> "LtP(" <> show p <> ")"
    NeP p -> "NeP(" <> show p <> ")"
    LikeP p -> "LikeP(" <> show p <> ")"


instance eqSimplePredicate :: Eq SimplePredicate where
  (==) (ContainsP p) (ContainsP p') = p == p'
  (==) (EqP p) (EqP p') = p == p'
  (==) (GtP p) (GtP p') = p == p'
  (==) (GteP p) (GteP p') = p == p'
  (==) (LteP p) (LteP p') = p == p'
  (==) (LtP p) (LtP p') = p == p'
  (==) (NeP p) (NeP p') = p == p'
  (==) (LikeP p) (LikeP p') = p == p'
  (==) _ _ = false
  (/=) a b = not $ a == b

data Label =
  Common String
  | Meta String

instance showLabel :: Show Label where
  show a = case a of
    Common p -> "Common(" <> show p <> ")"
    Meta p -> "Meta(" <> show p <> ")"

instance eqLabel :: Eq Label where
  (==) (Common p) (Common p') = p == p'
  (==) (Meta p) (Meta p') = p == p'
  (==) _ _ = false
  (/=) a b = not $ a == b

data SearchTermSimple =
  SimplePredicate SimplePredicate
  | InfieldPredicate [Label] SimplePredicate

instance showSearchTermSimpleEq :: Show SearchTermSimple where
  show a = case a of
    SimplePredicate p -> "SimplePredicate(" <> show p <> ")"
    InfieldPredicate ls p -> "InfieldPredicate(" <> show ls <> "," <> show p <> ")"

instance eqSearchTermSimple :: Eq SearchTermSimple where
  (==) (SimplePredicate p) (SimplePredicate p') = p == p'
  (==) (InfieldPredicate ls p) (InfieldPredicate ls' p') =
    p == p' && ls == ls'
  (==) _ _ = false
  (/=) a b = not $ a == b

data SearchTerm =
  IncludeTerm SearchTermSimple
  | ExcludeTerm SearchTermSimple

instance searchTermEq :: Eq SearchTerm where
  (==) (IncludeTerm t) (IncludeTerm t') = t == t'
  (==) (ExcludeTerm t) (ExcludeTerm t') = t == t'
  (==) _ _  = false
  (/=) a b = not $ a == b

instance showSearchTerm :: Show SearchTerm where
  show a = case a of
    IncludeTerm t -> "Include(" <> show t <> ")"
    ExcludeTerm t -> "Exclude(" <> show t <> ")"

data PredicateAndLabel =
  P SimplePredicate
  | L Label
  | I
  | E

instance showPredicateAndLabel :: Show PredicateAndLabel where
  show pl = case pl of
    P sp -> "P(" <> show sp <> ")"
    L l -> "L(" <> show l <> ")"
    I -> "I"
    E -> "E"


isP :: PredicateAndLabel -> Boolean
isP (P _) = true
isP _ = false

isL :: PredicateAndLabel -> Boolean
isL (L _) = true
isL _ = false

isI :: PredicateAndLabel -> Boolean
isI I = true
isI _ = false

isE :: PredicateAndLabel -> Boolean
isE E = true
isE _ = false 

i :: Parser [Value] PredicateAndLabel
i = get (Through Plus) *> pure I

e :: Parser [Value] PredicateAndLabel
e = get (Through Minus) *> pure E

l :: Parser [Value] PredicateAndLabel
l = do
  (try (when isLabel) >>= \(Label t) -> return $ L (Common t)) <|>
  (when isMeta >>= \(MetaLabel t) -> return $ L (Meta t))


containsP :: Parser [Value] SimplePredicate
containsP = ContainsP <$> when isVal

eqP :: Parser [Value] SimplePredicate
eqP =  get (Through Eq) *> (EqP <$> when isVal) 

gtP :: Parser [Value] SimplePredicate
gtP =  get (Through Gt) *> (GtP <$> when isVal) 

gteP :: Parser [Value] SimplePredicate
gteP = get (Through GtE) *> (GteP <$> when isVal) 

ltP :: Parser [Value] SimplePredicate
ltP = get (Through Lt) *> (LtP <$> when isVal) 

lteP :: Parser [Value] SimplePredicate
lteP = get (Through LtE) *> (LteP <$> when isVal) 

neP :: Parser [Value] SimplePredicate
neP = get (Through Ne) *> (NeP <$> when isVal) 

likeP :: Parser [Value] SimplePredicate
likeP = get (Through Tilde) *> (LikeP <$> when isVal) 
  
p :: Parser [Value] PredicateAndLabel
p = P <$> choice [try likeP,
                  try neP,
                  try lteP,
                  try ltP,
                  try gtP,
                  try gteP,
                  try eqP,
                  containsP]

predicatesAndLabels :: Parser [Value] [PredicateAndLabel]
predicatesAndLabels = many $ choice [try p, try l, i, e]

simpleTerm :: Parser [PredicateAndLabel] SearchTermSimple
simpleTerm = do
  ls <- try $ many (when isL >>= \(L l) -> return l)
  p <- when isP
  case Tuple ls p of
    Tuple [] (P p) -> return $ SimplePredicate p
    Tuple xs (P p) -> return $ InfieldPredicate xs p
    Tuple _ _ -> fail "impossible happens in simpleTerm"

searchTermI :: Parser [PredicateAndLabel] SearchTerm
searchTermI = do
  i <- option I (when isI)
  term <- simpleTerm
  return $ IncludeTerm term

searchTermE :: Parser [PredicateAndLabel] SearchTerm
searchTermE = do
  e <- when isE
  term <- simpleTerm
  return $ ExcludeTerm term

searchQuery :: Parser [PredicateAndLabel] [SearchTerm]
searchQuery = many $ choice [searchTermE, searchTermI]

search :: [Value] -> Either ParseError [SearchTerm]
search tokens =
  let parse = flip runParser in
  runParser tokens predicatesAndLabels >>= parse searchQuery

