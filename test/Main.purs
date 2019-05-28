module Test.Main where

import Prelude

import Data.Graph (topologicalSort, unfoldGraph)
import Data.Graph as Graph
import Data.List (toUnfoldable, range)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect, foreachE)
import Effect.Console (logShow)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = do
  let double x | x * 2 < 100000 = [x * 2]
               | otherwise      = []
      graph = unfoldGraph (range 1 100000) identity double
  foreachE (toUnfoldable (topologicalSort graph)) logShow
  run [consoleReporter] do
    let l = List.fromFoldable
        --       4 - 8
        --      /     \
        -- 1 - 2 - 3 - 5 - 7
        --          \
        --           6
        acyclicGraph =
          Graph.fromMap (
            Map.fromFoldable
              [ Tuple 1 (Tuple 1 (l [ 2 ]))
              , Tuple 2 (Tuple 2 (l [ 3, 4 ]))
              , Tuple 3 (Tuple 3 (l [ 5, 6 ]))
              , Tuple 4 (Tuple 4 (l [ 8 ]))
              , Tuple 5 (Tuple 5 (l [ 7 ]))
              , Tuple 6 (Tuple 6 (l [ ]))
              , Tuple 7 (Tuple 7 (l [ ]))
              , Tuple 8 (Tuple 8 (l [ 5 ]))
              ])
        --       2 - 4
        --      / \
        -- 5 - 1 - 3
        cyclicGraph =
          Graph.fromMap (
            Map.fromFoldable
              [ Tuple 1 (Tuple 1 (l [ 2 ]))
              , Tuple 2 (Tuple 2 (l [ 3, 4 ]))
              , Tuple 3 (Tuple 3 (l [ 1 ]))
              , Tuple 4 (Tuple 4 (l [ ]))
              , Tuple 5 (Tuple 5 (l [ 1 ]))
              ])
    describe "descendants" do
      it "works for examples" do
        Graph.descendants 1 acyclicGraph `shouldEqual` Set.fromFoldable [ 2, 3, 4, 5, 6, 7, 8 ]
        Graph.descendants 2 acyclicGraph `shouldEqual` Set.fromFoldable [ 3, 4, 5, 6, 7, 8 ]
        Graph.descendants 3 acyclicGraph `shouldEqual` Set.fromFoldable [ 5, 6, 7 ]
        Graph.descendants 4 acyclicGraph `shouldEqual` Set.fromFoldable [ 5, 7, 8 ]
        Graph.descendants 5 acyclicGraph `shouldEqual` Set.fromFoldable [ 7 ]
        Graph.descendants 6 acyclicGraph `shouldEqual` Set.fromFoldable [ ]
        Graph.descendants 7 acyclicGraph `shouldEqual` Set.fromFoldable [ ]
        Graph.descendants 8 acyclicGraph `shouldEqual` Set.fromFoldable [ 5, 7 ]
    describe "ancestors" do
      it "works for examples" do
        Graph.ancestors 1 acyclicGraph `shouldEqual` Set.fromFoldable [ ]
        Graph.ancestors 2 acyclicGraph `shouldEqual` Set.fromFoldable [ 1 ]
        Graph.ancestors 3 acyclicGraph `shouldEqual` Set.fromFoldable [ 1, 2 ]
        Graph.ancestors 4 acyclicGraph `shouldEqual` Set.fromFoldable [ 1, 2 ]
        Graph.ancestors 5 acyclicGraph `shouldEqual` Set.fromFoldable [ 1, 2, 3, 4, 8 ]
        Graph.ancestors 6 acyclicGraph `shouldEqual` Set.fromFoldable [ 1, 2, 3 ]
        Graph.ancestors 7 acyclicGraph `shouldEqual` Set.fromFoldable [ 1, 2, 3, 4, 5, 8 ]
        Graph.ancestors 8 acyclicGraph `shouldEqual` Set.fromFoldable [ 1, 2, 4 ]
    describe "adjacent" do
      it "works for examples" do
        Graph.adjacent 1 acyclicGraph `shouldEqual` Set.fromFoldable [ 2 ]
        Graph.adjacent 2 acyclicGraph `shouldEqual` Set.fromFoldable [ 1, 3, 4 ]
        Graph.adjacent 3 acyclicGraph `shouldEqual` Set.fromFoldable [ 2, 5, 6 ]
        Graph.adjacent 4 acyclicGraph `shouldEqual` Set.fromFoldable [ 2, 8 ]
        Graph.adjacent 5 acyclicGraph `shouldEqual` Set.fromFoldable [ 3, 7, 8 ]
        Graph.adjacent 6 acyclicGraph `shouldEqual` Set.fromFoldable [ 3 ]
        Graph.adjacent 7 acyclicGraph `shouldEqual` Set.fromFoldable [ 5 ]
        Graph.adjacent 8 acyclicGraph `shouldEqual` Set.fromFoldable [ 4, 5 ]
        Graph.adjacent 1 cyclicGraph `shouldEqual` Set.fromFoldable [ 2, 3, 5 ]
        Graph.adjacent 2 cyclicGraph `shouldEqual` Set.fromFoldable [ 1, 3, 4 ]
        Graph.adjacent 3 cyclicGraph `shouldEqual` Set.fromFoldable [ 1, 2 ]
        Graph.adjacent 4 cyclicGraph `shouldEqual` Set.fromFoldable [ 2 ]
        Graph.adjacent 5 cyclicGraph `shouldEqual` Set.fromFoldable [ 1 ]
    describe "path" do
      it "works for examples" do
        Graph.path 2 1 acyclicGraph `shouldEqual` Nothing
        Graph.path 1 9 acyclicGraph `shouldEqual` Nothing
        Graph.path 1 1 acyclicGraph `shouldEqual` Just (List.fromFoldable [ 1 ])
        Graph.path 1 2 acyclicGraph `shouldEqual` Just (List.fromFoldable [ 1, 2 ])
        Graph.path 1 7 acyclicGraph `shouldEqual` Just (List.fromFoldable [ 1, 2, 3, 5, 7 ])
        Graph.path 1 8 acyclicGraph `shouldEqual` Just (List.fromFoldable [ 1, 2, 4, 8 ])
        Graph.path 2 6 acyclicGraph `shouldEqual` Just (List.fromFoldable [ 2, 3, 6 ])
        Graph.path 5 3 cyclicGraph `shouldEqual` Just (List.fromFoldable [ 5, 1, 2, 3 ])
