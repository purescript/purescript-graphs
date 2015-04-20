-- | A data structure and functions for graphs

module Data.Graph (
  Edge(..),
  Graph(..),
  SCC(..),
  
  vertices,

  scc,
  scc',
  
  topSort,
  topSort'
  ) where

import Data.Int
import Data.Maybe
import Data.Array (map, reverse, concatMap)
import Data.Foldable
import Data.Traversable

import Control.Monad
import Control.Monad.Eff
import Control.Monad.ST

import qualified Data.Map as M
import qualified Data.Set as S

-- | An directed edge between vertices labelled with keys of type `k`.
data Edge k = Edge k k

-- | A graph with vertices of type `v`.
-- |
-- | Edges refer to vertices using keys of type `k`.
data Graph k v = Graph [v] [Edge k]

type Index = Int 

-- | A strongly-connected component of a graph.
-- |
-- | - `AcyclicSCC` identifies strongly-connected components consisting of a single vertex.
-- | - `CyclicSCC` identifies strongly-connected components with one or more vertices with
-- |   cycles.
data SCC v = AcyclicSCC v | CyclicSCC [v]

instance showSCC :: (Show v) => Show (SCC v) where
  show (AcyclicSCC v) = "AcyclicSCC (" ++ show v ++ ")" 
  show (CyclicSCC vs) = "CyclicSCC " ++ show vs

instance eqSCC :: (Eq v) => Eq (SCC v) where
  (==) (AcyclicSCC v1) (AcyclicSCC v2) = v1 == v2
  (==) (CyclicSCC vs1) (CyclicSCC vs2) = vs1 == vs2
  (==) _ _ = false
  (/=) scc1 scc2 = not (scc1 == scc2)

-- | Returns the vertices contained in a strongly-connected component.
vertices :: forall v. SCC v -> [v]
vertices (AcyclicSCC v) = [v]
vertices (CyclicSCC vs) = vs

-- | Compute the strongly connected components of a graph.
scc :: forall v. (Eq v, Ord v) => Graph v v -> [SCC v]
scc = scc' id id

-- | Compute the strongly connected components of a graph.
-- | 
-- | This function is a slight generalization of `scc` which allows key and value types
-- | to differ.
scc' :: forall k v. (Eq k, Ord k) => (v -> k) -> (k -> v) -> Graph k v -> [SCC v]
scc' makeKey makeVert (Graph vs es) = runPure (runST (do
  index      <- newSTRef zero 
  path       <- newSTRef []
  indexMap   <- newSTRef M.empty
  lowlinkMap <- newSTRef M.empty
  components <- newSTRef []

  (let 
    indexOf v = indexOfKey (makeKey v)
      
    indexOfKey k = do
      m <- readSTRef indexMap
      return $ M.lookup k m
    
    lowlinkOf v = lowlinkOfKey (makeKey v)
      
    lowlinkOfKey k = do
      m <- readSTRef lowlinkMap
      return $ M.lookup k m

    go [] = readSTRef components
    go (v : vs) = do
      currentIndex <- indexOf v
      when (isNothing currentIndex) $ strongConnect (makeKey v)
      go vs

    strongConnect k = do
      let v = makeVert k
      
      i <- readSTRef index

      modifySTRef indexMap   $ M.insert k i
      modifySTRef lowlinkMap $ M.insert k i

      writeSTRef index $ i + one
      modifySTRef path $ (:) v

      for es $ \(Edge k' l) -> when (k == k') $ do
        wIndex <- indexOfKey l
        currentPath <- readSTRef path

        case wIndex of
          Nothing -> do
            let w = makeVert l
            strongConnect l
            wLowlink <- lowlinkOfKey l
            for_ wLowlink $ \lowlink ->
              modifySTRef lowlinkMap $ M.alter (maybeMin lowlink) k
          _ -> when (l `elem` map makeKey currentPath) $ do
                 wIndex <- indexOfKey l
                 for_ wIndex $ \index ->
                   modifySTRef lowlinkMap $ M.alter (maybeMin index) k

      vIndex <- indexOfKey k
      vLowlink <- lowlinkOfKey k        

      when (vIndex == vLowlink) $ do
        currentPath <- readSTRef path
        let newPath = popUntil makeKey v currentPath []
        modifySTRef components $ flip (++) [makeComponent newPath.component]
        writeSTRef path newPath.path
        return unit
        
    makeComponent [v] | not (isCycle (makeKey v)) = AcyclicSCC v
    makeComponent vs = CyclicSCC vs
    
    isCycle k = any (\(Edge k1 k2) -> k1 == k && k2 == k) es
   in go vs)))

popUntil :: forall k v. (Eq k) => (v -> k) -> v -> [v] -> [v] -> { path :: [v], component :: [v] }
popUntil _       _ []         popped = { path: [], component: popped } 
popUntil makeKey v (w : path) popped | makeKey v == makeKey w = { path: path, component: w : popped }
popUntil makeKey v (w : ws)   popped = popUntil makeKey v ws (w : popped)

maybeMin :: Index -> Maybe Index -> Maybe Index
maybeMin i Nothing = Just i
maybeMin i (Just j) = Just $ fromNumber $ Math.min (toNumber i) (toNumber j)

-- | Topologically sort the vertices of a graph
topSort :: forall v. (Eq v, Ord v) => Graph v v -> [v]
topSort = topSort' id id

-- | Topologically sort the vertices of a graph
-- | 
-- | This function is a slight generalization of `scc` which allows key and value types
-- | to differ.
topSort' :: forall k v. (Eq k, Ord k) => (v -> k) -> (k -> v) -> Graph k v -> [v]
topSort' makeKey makeVert = reverse <<< concatMap vertices <<< scc' makeKey makeVert
