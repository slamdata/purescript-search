# Module Documentation

## Module Text.SlamSearch.Parser



#### `parseSearchQuery`

``` purescript
parseSearchQuery :: String -> Either ParseError [SearchTerm]
```



## Module Text.SlamSearch.Parser.Terms



#### `SimplePredicate`

``` purescript
data SimplePredicate
  = ContainsP Value
  | EqP Value
  | GtP Value
  | GteP Value
  | LteP Value
  | LtP Value
  | NeP Value
  | LikeP Value
```


#### `showSimplePredicate`

``` purescript
instance showSimplePredicate :: Show SimplePredicate
```


#### `eqSimplePredicate`

``` purescript
instance eqSimplePredicate :: Eq SimplePredicate
```


#### `Label`

``` purescript
data Label
  = Common String
  | Meta String
```


#### `showLabel`

``` purescript
instance showLabel :: Show Label
```


#### `eqLabel`

``` purescript
instance eqLabel :: Eq Label
```


#### `SearchTermSimple`

``` purescript
data SearchTermSimple
  = SimplePredicate SimplePredicate
  | InfieldPredicate [Label] SimplePredicate
```


#### `showSearchTermSimpleEq`

``` purescript
instance showSearchTermSimpleEq :: Show SearchTermSimple
```


#### `eqSearchTermSimple`

``` purescript
instance eqSearchTermSimple :: Eq SearchTermSimple
```


#### `SearchTerm`

``` purescript
data SearchTerm
  = IncludeTerm SearchTermSimple
  | ExcludeTerm SearchTermSimple
```


#### `searchTermEq`

``` purescript
instance searchTermEq :: Eq SearchTerm
```


#### `showSearchTerm`

``` purescript
instance showSearchTerm :: Show SearchTerm
```


#### `PredicateAndLabel`

``` purescript
data PredicateAndLabel
  = P SimplePredicate
  | L Label
  | I 
  | E 
```


#### `showPredicateAndLabel`

``` purescript
instance showPredicateAndLabel :: Show PredicateAndLabel
```


#### `isP`

``` purescript
isP :: PredicateAndLabel -> Boolean
```


#### `isL`

``` purescript
isL :: PredicateAndLabel -> Boolean
```


#### `isI`

``` purescript
isI :: PredicateAndLabel -> Boolean
```


#### `isE`

``` purescript
isE :: PredicateAndLabel -> Boolean
```


#### `i`

``` purescript
i :: Parser [Value] PredicateAndLabel
```


#### `e`

``` purescript
e :: Parser [Value] PredicateAndLabel
```


#### `l`

``` purescript
l :: Parser [Value] PredicateAndLabel
```


#### `containsP`

``` purescript
containsP :: Parser [Value] SimplePredicate
```


#### `eqP`

``` purescript
eqP :: Parser [Value] SimplePredicate
```


#### `gtP`

``` purescript
gtP :: Parser [Value] SimplePredicate
```


#### `gteP`

``` purescript
gteP :: Parser [Value] SimplePredicate
```


#### `ltP`

``` purescript
ltP :: Parser [Value] SimplePredicate
```


#### `lteP`

``` purescript
lteP :: Parser [Value] SimplePredicate
```


#### `neP`

``` purescript
neP :: Parser [Value] SimplePredicate
```


#### `likeP`

``` purescript
likeP :: Parser [Value] SimplePredicate
```


#### `p`

``` purescript
p :: Parser [Value] PredicateAndLabel
```


#### `predicatesAndLabels`

``` purescript
predicatesAndLabels :: Parser [Value] [PredicateAndLabel]
```


#### `simpleTerm`

``` purescript
simpleTerm :: Parser [PredicateAndLabel] SearchTermSimple
```


#### `searchTermI`

``` purescript
searchTermI :: Parser [PredicateAndLabel] SearchTerm
```


#### `searchTermE`

``` purescript
searchTermE :: Parser [PredicateAndLabel] SearchTerm
```


#### `searchQuery`

``` purescript
searchQuery :: Parser [PredicateAndLabel] [SearchTerm]
```


#### `search`

``` purescript
search :: [Value] -> Either ParseError [SearchTerm]
```



## Module Text.SlamSearch.Parser.Tokens



#### `rawString`

``` purescript
rawString :: Parser String String
```


#### `slashed`

``` purescript
slashed :: Parser String String
```


#### `quotedSymbol`

``` purescript
quotedSymbol :: Parser String String
```


#### `quotedString`

``` purescript
quotedString :: Parser String String
```


#### `Token`

``` purescript
data Token
  = Text String
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
```


#### `showToken`

``` purescript
instance showToken :: Show Token
```


#### `eqToken`

``` purescript
instance eqToken :: Eq Token
```


#### `isText`

``` purescript
isText :: Token -> Boolean
```


#### `raw`

``` purescript
raw :: Parser String Token
```


#### `quoted`

``` purescript
quoted :: Parser String Token
```


#### `star`

``` purescript
star :: Parser String Token
```


#### `range`

``` purescript
range :: Parser String Token
```


#### `qmark`

``` purescript
qmark :: Parser String Token
```


#### `hash`

``` purescript
hash :: Parser String Token
```


#### `plus`

``` purescript
plus :: Parser String Token
```


#### `minus`

``` purescript
minus :: Parser String Token
```


#### `at`

``` purescript
at :: Parser String Token
```


#### `eq`

``` purescript
eq :: Parser String Token
```


#### `lt`

``` purescript
lt :: Parser String Token
```


#### `gt`

``` purescript
gt :: Parser String Token
```


#### `lte`

``` purescript
lte :: Parser String Token
```


#### `gte`

``` purescript
gte :: Parser String Token
```


#### `ne`

``` purescript
ne :: Parser String Token
```


#### `tilde`

``` purescript
tilde :: Parser String Token
```


#### `colon`

``` purescript
colon :: Parser String Token
```


#### `tokenize`

``` purescript
tokenize :: Parser String [Token]
```


#### `tokens`

``` purescript
tokens :: String -> Either ParseError [Token]
```



## Module Text.SlamSearch.Parser.Utils



#### `takeTok`

``` purescript
takeTok :: forall m a. (Monad m) => ParserT [a] m a
```


#### `when`

``` purescript
when :: forall m a. (Monad m) => (a -> Boolean) -> ParserT [a] m a
```


#### `get`

``` purescript
get :: forall a m. (Monad m, Eq a) => a -> ParserT [a] m a
```



## Module Text.SlamSearch.Parser.Values



#### `Value`

``` purescript
data Value
  = Value String
  | RangeVal String String
  | Tag String
  | Label String
  | MetaLabel String
  | Glob String
  | Through Token
```


#### `isValue`

``` purescript
isValue :: Value -> Boolean
```


#### `isRangeVal`

``` purescript
isRangeVal :: Value -> Boolean
```


#### `isTag`

``` purescript
isTag :: Value -> Boolean
```


#### `isLabel`

``` purescript
isLabel :: Value -> Boolean
```


#### `isMeta`

``` purescript
isMeta :: Value -> Boolean
```


#### `isGlob`

``` purescript
isGlob :: Value -> Boolean
```


#### `isVal`

``` purescript
isVal :: Value -> Boolean
```


#### `valueShow`

``` purescript
instance valueShow :: Show Value
```


#### `valueEq`

``` purescript
instance valueEq :: Eq Value
```


#### `text`

``` purescript
text :: Parser [Token] String
```


#### `tag`

``` purescript
tag :: Parser [Token] Value
```


#### `label`

``` purescript
label :: Parser [Token] Value
```


#### `meta`

``` purescript
meta :: Parser [Token] Value
```


#### `rangeVal`

``` purescript
rangeVal :: Parser [Token] Value
```


#### `globSymb`

``` purescript
globSymb :: Parser [Token] String
```


#### `globTextP`

``` purescript
globTextP :: Parser [Token] String
```


#### `globTextA`

``` purescript
globTextA :: Parser [Token] String
```


#### `glob`

``` purescript
glob :: Parser [Token] Value
```


#### `simple`

``` purescript
simple :: Parser [Token] Value
```


#### `through`

``` purescript
through :: Parser [Token] Value
```


#### `vals`

``` purescript
vals :: Parser [Token] [Value]
```


#### `values`

``` purescript
values :: [Token] -> Either ParseError [Value]
```




