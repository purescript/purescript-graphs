## Module Data.Graph

A data structure and functions for graphs

#### `Edge`

``` purescript
data Edge k
  = Edge k k
```

An directed edge between vertices labelled with keys of type `k`.

#### `Graph`

``` purescript
data Graph k v
  = Graph (List v) (List (Edge k))
```

A graph with vertices of type `v`.

Edges refer to vertices using keys of type `k`.

#### `SCC`

``` purescript
data SCC v
  = AcyclicSCC v
  | CyclicSCC (List v)
```

A strongly-connected component of a graph.

- `AcyclicSCC` identifies strongly-connected components consisting of a single vertex.
- `CyclicSCC` identifies strongly-connected components with one or more vertices with
  cycles.

##### Instances
``` purescript
instance showSCC :: (Show v) => Show (SCC v)
instance eqSCC :: (Eq v) => Eq (SCC v)
```

#### `vertices`

``` purescript
vertices :: forall v. SCC v -> List v
```

Returns the vertices contained in a strongly-connected component.

#### `scc`

``` purescript
scc :: forall v. (Eq v, Ord v) => Graph v v -> List (SCC v)
```

Compute the strongly connected components of a graph.

#### `scc'`

``` purescript
scc' :: forall k v. (Eq k, Ord k) => (v -> k) -> (k -> v) -> Graph k v -> List (SCC v)
```

Compute the strongly connected components of a graph.

This function is a slight generalization of `scc` which allows key and value types
to differ.

#### `topSort`

``` purescript
topSort :: forall v. (Eq v, Ord v) => Graph v v -> List v
```

Topologically sort the vertices of a graph

#### `topSort'`

``` purescript
topSort' :: forall k v. (Eq k, Ord k) => (v -> k) -> (k -> v) -> Graph k v -> List v
```

Topologically sort the vertices of a graph

This function is a slight generalization of `scc` which allows key and value types
to differ.


