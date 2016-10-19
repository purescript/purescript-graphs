-- | A data structure and functions for graphs

module Data.Graph
  ( Graph
  , unfoldGraph
  , vertices
  , lookup
  , outEdges
  , topologicalSort
  ) where

import Prelude
import Data.Foldable (class Foldable, foldl)
import Data.List (List(..))
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))

-- | A graph with vertices of type `v`.
-- |
-- | Edges refer to vertices using keys of type `k`.
newtype Graph k v = Graph
  { vertices :: Map k { value :: v, out :: List k }
  }

-- | Unfold a graph from a collection of keys and functions which label keys
-- | and specify out-edges.
unfoldGraph
  :: forall f k v out
   . (Ord k, Functor f, Foldable f, Foldable out)
  => f k
  -> (k -> v)
  -> (k -> out k)
  -> Graph k v
unfoldGraph ks label edges =
  Graph { vertices: M.fromFoldable (map (\k ->
            Tuple k { value: label k
                    , out: L.fromFoldable (edges k)
                    }) ks)
        }

-- | List all vertices in a graph.
vertices :: forall k v. Graph k v -> List v
vertices (Graph g) = map _.value (M.values g.vertices)

-- | Lookup a vertex by its key.
lookup :: forall k v. Ord k => k -> Graph k v -> Maybe v
lookup k (Graph g) = map _.value (M.lookup k g.vertices)

-- | Get the keys which are directly accessible from the given key.
outEdges :: forall k v. Ord k => k -> Graph k v -> Maybe (List k)
outEdges k (Graph g) = map _.out (M.lookup k g.vertices)

type SortState k v =
  { unvisited :: Map k { value :: v, out :: List k }
  , result :: List k
  }

-- | Topologically sort the vertices of a graph.
-- |
-- | If the graph contains cycles, then the behavior is undefined.
topologicalSort :: forall k v. Ord k => Graph k v -> List k
topologicalSort (Graph g) =
    go initialState
  where
    go :: SortState k v -> List k
    go state@{ unvisited, result } =
      case M.findMin unvisited of
        Just { key } -> go (visit state key)
        Nothing -> result

    visit :: SortState k v -> k -> SortState k v
    visit state k
      | k `M.member` state.unvisited =
        let finish :: SortState k v -> SortState k v
            finish { unvisited, result } =
              { result: Cons k result
              , unvisited: unvisited
              }

            start :: SortState k v
            start =
              { result: state.result
              , unvisited: M.delete k state.unvisited
              }
        in finish (foldl visit start (maybe mempty _.out (M.lookup k g.vertices)))
      | otherwise = state

    initialState :: SortState k v
    initialState = { unvisited: g.vertices
                   , result: Nil
                   }
