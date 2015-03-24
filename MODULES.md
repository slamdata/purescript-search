# Module Documentation

## Module Text.SlamSearch

#### `mkQuery`

``` purescript
mkQuery :: String -> SearchQuery
```


#### `check`

``` purescript
check :: forall a. a -> SearchQuery -> (a -> Term -> Boolean) -> Boolean
```



## Module Text.SlamSearch.Parser

#### `term`

``` purescript
term :: P.Parser [Tk.Token] S.Term
```



## Module Text.SlamSearch.Printer

#### `strLabel`

``` purescript
strLabel :: Label -> String
```


#### `strValue`

``` purescript
strValue :: Value -> String
```


#### `strPredicate`

``` purescript
strPredicate :: Predicate -> String
```


#### `strTerm`

``` purescript
strTerm :: Term -> String
```


#### `strQuery`

``` purescript
strQuery :: SearchQuery -> String
```



## Module Text.SlamSearch.Types

#### `SearchQuery`

``` purescript
type SearchQuery = Free Term
```

SearchQuery is free semiring on Term

#### `Term`

``` purescript
newtype Term
  = Term { predicate :: Predicate, labels :: [Label], include :: Boolean }
```


#### `showTerm`

``` purescript
instance showTerm :: Show Term
```


#### `eqTerm`

``` purescript
instance eqTerm :: Eq Term
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


#### `Predicate`

``` purescript
data Predicate
  = Contains Value
  | Eq Value
  | Gt Value
  | Gte Value
  | Lt Value
  | Lte Value
  | Ne Value
  | Like String
```


#### `eqPredicate`

``` purescript
instance eqPredicate :: Eq Predicate
```


#### `showPredicate`

``` purescript
instance showPredicate :: Show Predicate
```


#### `Value`

``` purescript
data Value
  = Text String
  | Range String String
  | Tag String
```


#### `eqValue`

``` purescript
instance eqValue :: Eq Value
```


#### `showValue`

``` purescript
instance showValue :: Show Value
```



## Module Text.SlamSearch.Parser.Tokens

#### `Token`

``` purescript
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




