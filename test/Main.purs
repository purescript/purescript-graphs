module Test.Main where

import Prelude

import Data.Foldable (foldr)
import Data.Graph (Graph, Edge(..), edges, fromMap, unfoldGraph, topologicalSort)
import Data.List (filter, length, toUnfoldable, range, (:), List(Nil))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(Just))
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect, foreachE)
import Effect.Console (logShow)

-- An example graph
--       0
--      / \
--     1  2
--      \/
--      3

example1 :: Graph Int Int
example1 =
  fromMap example1'
  where
  example1' :: Map Int (Tuple Int (List Int))
  example1' =
    M.fromFoldable
      [ (0 /\ (0 /\ (1 : 2 : Nil)))
      , (1 /\ (1 /\ (3 : Nil)))
      , (2 /\ (2 /\ (3 : Nil)))
      , (3 /\ (3 /\ Nil))
      ]

showEdge :: forall k. Show k => Edge k -> String
showEdge (Edge ends) =
  show ends

main :: Effect Unit
main = do
  let
    double x
      | x * 2 < 100000 = [ x * 2 ]
      | otherwise = []

    graph :: Graph Int Int
    graph = unfoldGraph (range 1 100000) identity double
  foreachE (toUnfoldable (topologicalSort graph)) logShow
  logShow
    $ filter (_ /= 0) <<< foldr (:) Nil
        <$> traverse (\n -> if n `mod` 2 == 0 then Just 0 else Just n) graph
  logShow (length (edges graph))
  logShow $ map showEdge $ edges example1
