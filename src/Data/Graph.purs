-- | A data structure and functions for graphs

module Data.Graph
  ( Graph
  , unfoldGraph
  , fromMap
  , toMap
  , empty
  , insertEdge
  , insertVertex
  , insertEdgeWithVertices
  , vertices
  , lookup
  , outEdges
  , children
  , descendants
  , parents
  , ancestors
  , topologicalSort
  , isInCycle
  , isCyclic
  , isAcyclic
  , adjacent
  , isAdjacent
  , areConnected
  , path
  ) where

import Prelude

import Data.Bifunctor (lmap, rmap)
import Data.CatList (CatList)
import Data.CatList as CL
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.List (List(..))
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Set (Set)
import Data.Set as S
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd, uncurry)

-- | A graph with vertices of type `v`.
-- |
-- | Edges refer to vertices using keys of type `k`.
newtype Graph k v = Graph (Map k (Tuple v (Set k)))

instance functorGraph :: Functor (Graph k) where
  map f (Graph m) = Graph (map (lmap f) m)

-- | An empty graph.
empty :: forall k v. Graph k v
empty = Graph M.empty

-- | Insert an edge from the start key to the end key.
insertEdge :: forall k v. Ord k => k -> k -> Graph k v -> Graph k v
insertEdge from to (Graph g) =
  Graph $ M.alter (map (rmap (S.insert to))) from g

-- | Insert a vertex into the graph.
-- |
-- | If the key already exists, replaces the existing value and
-- |preserves existing edges.
insertVertex :: forall k v. Ord k => k -> v -> Graph k v -> Graph k v
insertVertex k v (Graph g) = Graph $ M.insertWith (\(Tuple _ ks) _ -> Tuple v ks) k (Tuple v mempty) g

-- | Insert two vertices and connect them.
insertEdgeWithVertices :: forall k v. Ord k => Tuple k v -> Tuple k v -> Graph k v -> Graph k v
insertEdgeWithVertices from@(Tuple fromKey _) to@(Tuple toKey _) =
  insertEdge fromKey toKey <<< uncurry insertVertex from <<< uncurry insertVertex to

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
            Tuple k (Tuple (label k) (S.fromFoldable (edges k)))) ks))

-- | Create a `Graph` from a `Map` which maps vertices to their labels and
-- | outgoing edges.
fromMap :: forall k v. Map k (Tuple v (Set k)) -> Graph k v
fromMap = Graph

-- | Turn a `Graph` into a `Map` which maps vertices to their labels and
-- | outgoing edges.
toMap :: forall k v. Graph k v -> Map k (Tuple v (Set k))
toMap (Graph g) = g

-- | Check if the first key is adjacent to the second.
isAdjacent :: forall k v. Ord k => k -> k -> Graph k v -> Boolean
isAdjacent k1 k2 g = k1 `Set.member` adjacent k2 g

-- | Find all keys adjacent to given key.
adjacent :: forall k v. Ord k => k -> Graph k v -> Set k
adjacent k g = children k g `Set.union` parents k g

-- | Returns shortest path between start and end key if it exists.
-- |
-- | Will return bottom if the path includes but doesn't end on a cycle.
path :: forall k v. Ord k => k -> k -> Graph k v -> Maybe (List k)
path start end (Graph g) = L.reverse <$> go mempty start
  where
    go hist k =
      if end == k
      then Just hist'
      else
        case M.lookup k g of
          Nothing -> Nothing
          Just (Tuple _ ks) ->
            S.findMin <<< S.mapMaybe identity $ Set.map (go hist') ks
      where
        hist' = k `Cons` hist

-- | Checks if there's a directed path between the start and end key.
areConnected :: forall k v. Ord k => k -> k -> Graph k v -> Boolean
areConnected start end g = isJust $ path start end g

-- | List all vertices in a graph.
vertices :: forall k v. Graph k v -> List v
vertices (Graph g) = map fst (M.values g)

-- | Lookup a vertex by its key.
lookup :: forall k v. Ord k => k -> Graph k v -> Maybe v
lookup k (Graph g) = map fst (M.lookup k g)

-- | Get the keys which are directly accessible from the given key.
outEdges :: forall k v. Ord k => k -> Graph k v -> Maybe (Set k)
outEdges k (Graph g) = map snd (M.lookup k g)

-- | Returns immediate ancestors of given key.
parents :: forall k v. Ord k => k -> Graph k v -> Set k
parents k (Graph g) = M.keys <<< M.filter (Foldable.elem k <<< snd) $ g

-- | Returns all ancestors of given key.
-- |
-- | Will return bottom if `k` is in cycle.
ancestors :: forall k v. Ord k => k -> Graph k v -> Set k
ancestors k' g = go k'
  where
   go k = Set.unions $ Set.insert da $ Set.map go da
     where
       da = parents k g

-- | Returns immediate descendants of given key.
children :: forall k v. Ord k => k -> Graph k v -> Set k
children k (Graph g) = maybe mempty (Set.fromFoldable <<< snd) <<< M.lookup k $ g

-- | Returns all descendants of given key.
-- |
-- | Will return bottom if `k` is in cycle.
descendants :: forall k v. Ord k => k -> Graph k v -> Set k
descendants k' g = go k'
  where
   go k = Set.unions $ Set.insert dd $ Set.map go dd
     where
       dd = children k g

-- | Checks if given key is part of a cycle.
isInCycle :: forall k v. Ord k => k -> Graph k v -> Boolean
isInCycle k' g = go mempty k'
  where
    go seen k =
      case Tuple (dd == mempty) (k `Set.member` seen) of
        Tuple true _ -> false
        Tuple _ true -> k == k'
        Tuple false false -> Foldable.any (go (Set.insert k seen)) dd
      where
        dd = children k g

-- | Checks if there any cycles in graph.
-- There's presumably a faster implementation but this is very easy to implement
isCyclic :: forall k v. Ord k => Graph k v -> Boolean
isCyclic g = Foldable.any (flip isInCycle g) <<< keys $ g
  where
    keys (Graph g') = M.keys g'

-- | Checks if there are not any cycles in the graph.
isAcyclic :: forall k v. Ord k => Graph k v -> Boolean
isAcyclic = not <<< isCyclic

type SortState k v =
  { unvisited :: Map k (Tuple v (Set k))
  , result :: List k
  }

-- To defunctionalize the `topologicalSort` function and make it tail-recursive,
-- we introduce this data type which captures what we intend to do at each stage
-- of the recursion.
data SortStep a = Emit a | Visit a
derive instance eqSortStep :: Eq a => Eq (SortStep a)
derive instance ordSortStep :: Ord a => Ord (SortStep a)

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

                next :: Set k
                next = maybe mempty snd (M.lookup k g)
            in visit start (CL.fromFoldable (Set.map Visit next) <> CL.cons (Emit k) ks)
          | otherwise -> visit state ks

    initialState :: SortState k v
    initialState = { unvisited: g
                   , result: Nil
                   }
