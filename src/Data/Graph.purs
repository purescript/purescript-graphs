-- | A data structure and functions for graphs

module Data.Graph
  ( Graph
  , unfoldGraph
  , fromMap
  , vertices
  , lookup
  , outEdges
  , topologicalSort
  ) where

import Prelude
import Data.Bifunctor (lmap)
import Data.CatList (CatList)
import Data.CatList as CL
import Data.Foldable (class Foldable)
import Data.List (List(..))
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst, snd)

-- | A graph with vertices of type `v`.
-- |
-- | Edges refer to vertices using keys of type `k`.
newtype Graph k v = Graph (Map k (Tuple v (List k)))

instance functorGraph :: Functor (Graph k) where
  map f (Graph m) = Graph (map (lmap f) m)

-- | Unfold a `Graph` from a collection of keys and functions which label keys
-- | and specify out-edges.
unfoldGraph
  :: forall f k v out
   . Ord k
  => Functor f
  => Foldable f
  => Foldable out
  => f k
  -> (k -> v)
  -> (k -> out k)
  -> Graph k v
unfoldGraph ks label edges =
  Graph (M.fromFoldable (map (\k ->
            Tuple k (Tuple (label k) (L.fromFoldable (edges k)))) ks))

-- | Create a `Graph` from a `Map` which maps vertices to their labels and
-- | outgoing edges.
fromMap :: forall k v. Map k (Tuple v (List k)) -> Graph k v
fromMap = Graph

-- | List all vertices in a graph.
vertices :: forall k v. Graph k v -> List v
vertices (Graph g) = map fst (M.values g)

-- | Lookup a vertex by its key.
lookup :: forall k v. Ord k => k -> Graph k v -> Maybe v
lookup k (Graph g) = map fst (M.lookup k g)

-- | Get the keys which are directly accessible from the given key.
outEdges :: forall k v. Ord k => k -> Graph k v -> Maybe (List k)
outEdges k (Graph g) = map snd (M.lookup k g)

type SortState k v =
  { unvisited :: Map k (Tuple v (List k))
  , result :: List k
  }

-- To defunctionalize the `topologicalSort` function and make it tail-recursive,
-- we introduce this data type which captures what we intend to do at each stage
-- of the recursion.
data SortStep a = Emit a | Visit a

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
        Just { key } -> go (visit state (CL.fromFoldable [Visit key]))
        Nothing -> result

    visit :: SortState k v -> CatList (SortStep k) -> SortState k v
    visit state stack =
      case CL.uncons stack of
        Nothing -> state
        Just (Tuple (Emit k) ks) ->
          let state' = { result: Cons k state.result
                       , unvisited: state.unvisited
                       }
          in visit state' ks
        Just (Tuple (Visit k) ks)
          | k `M.member` state.unvisited ->
            let start :: SortState k v
                start =
                  { result: state.result
                  , unvisited: M.delete k state.unvisited
                  }

                next :: List k
                next = maybe mempty snd (M.lookup k g)
            in visit start (CL.fromFoldable (map Visit next) <> CL.cons (Emit k) ks)
          | otherwise -> visit state ks

    initialState :: SortState k v
    initialState = { unvisited: g
                   , result: Nil
                   }
