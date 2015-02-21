# Module Documentation

## Module Data.Graph

#### `Edge`

``` purescript
data Edge k
  = Edge k k
```


#### `Graph`

``` purescript
data Graph k v
  = Graph [v] [Edge k]
```


#### `SCC`

``` purescript
data SCC v
  = AcyclicSCC v
  | CyclicSCC [v]
```


#### `showSCC`

``` purescript
instance showSCC :: (Show v) => Show (SCC v)
```


#### `eqSCC`

``` purescript
instance eqSCC :: (Eq v) => Eq (SCC v)
```


#### `vertices`

``` purescript
vertices :: forall v. SCC v -> [v]
```


#### `scc`

``` purescript
scc :: forall v. (Eq v, Ord v) => Graph v v -> [SCC v]
```


#### `scc'`

``` purescript
scc' :: forall k v. (Eq k, Ord k) => (v -> k) -> (k -> v) -> Graph k v -> [SCC v]
```


#### `topSort`

``` purescript
topSort :: forall v. (Eq v, Ord v) => Graph v v -> [v]
```

#### `topSort'`

``` purescript
topSort' :: forall k v. (Eq k, Ord k) => (v -> k) -> (k -> v) -> Graph k v -> [v]
```