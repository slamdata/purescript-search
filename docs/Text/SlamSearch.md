## Module Text.SlamSearch

#### `mkQuery`

``` purescript
mkQuery :: String -> Either ParseError SearchQuery
```

#### `check`

``` purescript
check :: forall a. a -> SearchQuery -> (a -> Term -> Boolean) -> Boolean
```


