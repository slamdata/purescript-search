# Module Documentation

## Module Text.SlamSearch.Parser



#### `SearchQuery`

``` purescript
data SearchQuery
  = EmptyQuery 
  | SearchAnd SearchTerm SearchQuery
```


#### `showSearchQuery`

``` purescript
instance showSearchQuery :: Show SearchQuery
```


#### `eqSearchQuery`

``` purescript
instance eqSearchQuery :: Eq SearchQuery
```


#### `parseSearchQuery`

``` purescript
parseSearchQuery :: String -> Either ParseError SearchQuery
```


#### `parseSearchTerm`

``` purescript
parseSearchTerm :: String -> Either ParseError SearchTerm
```



## Module Text.SlamSearch.Printer



#### `prettyQuery`

``` purescript
prettyQuery :: SearchQuery -> String
```



## Module Text.SlamSearch.Parser.Terms



#### `Predicate`

``` purescript
data Predicate
  = ContainsPredicate Value
  | EqPredicate Value
  | GtPredicate Value
  | GtePredicate Value
  | LtePredicate Value
  | LtPredicate Value
  | NePredicate Value
  | LikePredicate Value
```


#### `showPredicate`

``` purescript
instance showPredicate :: Show Predicate
```


#### `predicateEq`

``` purescript
instance predicateEq :: Eq Predicate
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
  = SearchTermSimple [Label] Predicate
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


#### `predicateAndLabelEq`

``` purescript
instance predicateAndLabelEq :: Eq PredicateAndLabel
```


#### `showPredicateAndLabel`

``` purescript
instance showPredicateAndLabel :: Show PredicateAndLabel
```


#### `search`

``` purescript
search :: [Value] -> Either ParseError SearchTerm
```



## Module Text.SlamSearch.Parser.Tokens



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


#### `tokens`

``` purescript
tokens :: String -> Either ParseError [Token]
```



## Module Text.SlamSearch.Parser.Values



#### `Value`

``` purescript
data Value
  = TextVal String
  | RangeVal String String
  | Tag String
  | Label String
  | MetaLabel String
  | Glob String
  | Through Token
```


#### `isLabel`

``` purescript
isLabel :: Value -> Boolean
```


#### `isMeta`

``` purescript
isMeta :: Value -> Boolean
```


#### `isTextual`

``` purescript
isTextual :: Value -> Boolean
```


#### `valueShow`

``` purescript
instance valueShow :: Show Value
```


#### `valueEq`

``` purescript
instance valueEq :: Eq Value
```


#### `values`

``` purescript
values :: [Token] -> Either ParseError [Value]
```




