module Test.Main where

import Prelude

import Effect (Effect, foreachE)
import Effect.Console (logShow)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(Just))
import Data.Traversable (traverse)
import Data.Graph (Graph, unfoldGraph, topologicalSort)
import Data.List (filter, toUnfoldable, range, (:), List(Nil))

main :: Effect Unit
main = do
  let double x | x * 2 < 100000 = [x * 2]
               | otherwise      = []
      graph :: Graph Int Int
      graph = unfoldGraph (range 1 100000) identity double
  foreachE (toUnfoldable (topologicalSort graph)) logShow
  logShow
    $ filter (_ /= 0) <<< foldr (:) Nil
    <$> traverse (\n -> if n `mod` 2 == 0 then Just 0 else Just n) graph

