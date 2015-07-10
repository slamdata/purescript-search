## Module Text.SlamSearch.Types

#### `SearchQuery`

``` purescript
type SearchQuery = Free Term
```

SearchQuery is free semiring on Term

#### `Term`

``` purescript
newtype Term
  = Term { include :: Boolean, labels :: List Label, predicate :: Predicate }
```

##### Instances
``` purescript
instance showTerm :: Show Term
instance eqTerm :: Eq Term
```

#### `Label`

``` purescript
data Label
  = Common String
  | Meta String
```

##### Instances
``` purescript
instance showLabel :: Show Label
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

##### Instances
``` purescript
instance eqPredicate :: Eq Predicate
instance showPredicate :: Show Predicate
```

#### `Value`

``` purescript
data Value
  = Text String
  | Range String String
  | Tag String
```

##### Instances
``` purescript
instance eqValue :: Eq Value
instance showValue :: Show Value
```


