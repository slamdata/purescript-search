## Module Data.Semiring.Disjunctive

#### `Disjunctive`

``` purescript
newtype Disjunctive
  = Disjunctive Boolean
```

Boolean semiring in disjunctive normal form

```purescript
Disjunctive x + Disjunctive y = Disjunctive (x || y)
Disjunctive x * Disjunctive y = Disjunctive (x && y) 
zero :: Disjunctive = Disjunctive false
one :: Disjunctive = Disjunctive true
```

##### Instances
``` purescript
instance eqDisjunctive :: Eq Disjunctive
instance ordDisjunctive :: Ord Disjunctive
instance showDisjunctive :: Show Disjunctive
instance boundedDisjunctive :: Bounded Disjunctive
instance booleanAlgebraDisjunctive :: BooleanAlgebra Disjunctive
instance semiringDisjunctive :: Semiring Disjunctive
```

#### `runDisjunctive`

``` purescript
runDisjunctive :: Disjunctive -> Boolean
```


