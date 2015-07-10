## Module Text.SlamSearch.Parser.Tokens

#### `keyChars`

``` purescript
keyChars :: Array Char
```

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

##### Instances
``` purescript
instance showToken :: Show Token
instance eqToken :: Eq Token
```

#### `isText`

``` purescript
isText :: Token -> Boolean
```

#### `tokens`

``` purescript
tokens :: String -> Either ParseError (List Token)
```


