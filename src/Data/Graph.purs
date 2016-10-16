-- | A data structure and functions for graphs

module Data.Graph
  ( Graph(..)
  , topSort
  ) where

import Prelude

import Data.Foldable (foldl)
import Data.List (List(..))
import Data.Map (Map, lookup, delete, findMin)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)

-- | A graph with vertices of type `v`.
-- |
-- | Edges refer to vertices using keys of type `k`.
newtype Graph k v = Graph
  { vertices :: Map k { value :: v, out :: List k }
  }

type SortState k =
  { unvisited :: Map k Unit
  , result :: List k
  }

-- | Topologically sort the vertices of a graph
topSort :: forall k v. Ord k => Graph k v -> List k
topSort (Graph { vertices }) =
    go initialState
  where
    go :: SortState k -> List k
    go state@{ unvisited, result } =
      case findMin unvisited of
        Just { key } -> go (visit state key)
        Nothing -> result

    visit :: SortState k -> k -> SortState k
    visit state k =
      let finish :: SortState k -> SortState k
          finish { unvisited, result } =
            { result: Cons k result
            , unvisited: delete k unvisited
            }
      in finish (foldl visit state (maybe mempty _.out (lookup k vertices)))

    initialState :: SortState k
    initialState = { unvisited: void vertices
                   , result: Nil
                   }
