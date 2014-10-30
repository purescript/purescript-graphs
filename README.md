# Module Documentation

## Module Data.Graph

### Types

    data Edge k where
      Edge :: k -> k -> Edge k

    data Graph k v where
      Graph :: [v] -> [Edge k] -> Graph k v

    data SCC v where
      AcyclicSCC :: v -> SCC v
      CyclicSCC :: [v] -> SCC v


### Type Class Instances

    instance eqSCC :: (Eq v) => Eq (SCC v)

    instance showSCC :: (Show v) => Show (SCC v)


### Values

    scc :: forall v. (Eq v, Ord v) => Graph v v -> [SCC v]

    scc' :: forall k v. (Eq k, Ord k) => (v -> k) -> (k -> v) -> Graph k v -> [SCC v]

    topSort :: forall v. (Eq v, Ord v) => Graph v v -> [v]

    topSort' :: forall k v. (Eq k, Ord k) => (v -> k) -> (k -> v) -> Graph k v -> [v]

    vertices :: forall v. SCC v -> [v]