# Module Documentation

## Module Text.SlamSearch

#### `mkQuery`

``` purescript
mkQuery :: String -> SearchQuery
```


#### `semiringBool`

``` purescript
instance semiringBool :: Semiring Boolean
```


#### `check`

``` purescript
check :: forall a. a -> SearchQuery -> (a -> Term -> Boolean) -> Boolean
```



## Module Text.SlamSearch.Parser

#### `text`

``` purescript
text :: P.Parser [Tk.Token] String
```


#### `label`

``` purescript
label :: P.Parser [Tk.Token] S.Label
```


#### `meta`

``` purescript
meta :: P.Parser [Tk.Token] S.Label
```


#### `slabel`

``` purescript
slabel :: P.Parser [Tk.Token] S.Label
```


#### `tag`

``` purescript
tag :: P.Parser [Tk.Token] S.Value
```


#### `range`

``` purescript
range :: P.Parser [Tk.Token] S.Value
```


#### `val`

``` purescript
val :: P.Parser [Tk.Token] S.Value
```


#### `svalue`

``` purescript
svalue :: P.Parser [Tk.Token] S.Value
```


#### `PredicateParser`

``` purescript
type PredicateParser = P.Parser [Tk.Token] S.Predicate
```


#### `contains`

``` purescript
contains :: PredicateParser
```


#### `eq`

``` purescript
eq :: PredicateParser
```


#### `gt`

``` purescript
gt :: PredicateParser
```


#### `gte`

``` purescript
gte :: PredicateParser
```


#### `lt`

``` purescript
lt :: PredicateParser
```


#### `lte`

``` purescript
lte :: PredicateParser
```


#### `ne`

``` purescript
ne :: PredicateParser
```


#### `like`

``` purescript
like :: PredicateParser
```


#### `predicate`

``` purescript
predicate :: PredicateParser
```


#### `term`

``` purescript
term :: P.Parser [Tk.Token] S.Term
```



## Module Text.SlamSearch.Printer


## Module Text.SlamSearch.Types

#### `SearchQuery`

``` purescript
type SearchQuery = Free Term
```

SearchQuery is free semiring on Term

#### `Term`

``` purescript
type Term = { predicate :: Predicate, labels :: [Label], include :: Boolean }
```


#### `Label`

``` purescript
data Label
  = Common String
  | Meta String
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


#### `Value`

``` purescript
data Value
  = Text String
  | Range String String
  | Tag String
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




