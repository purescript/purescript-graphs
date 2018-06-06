module Test.Main where

import Prelude

import Effect (Effect, foreachE)
import Effect.Console (logShow)
import Data.Graph (unfoldGraph, topologicalSort)
import Data.List (toUnfoldable, range)

main :: Effect Unit
main = do
  let double x | x * 2 < 100000 = [x * 2]
               | otherwise      = []
      graph = unfoldGraph (range 1 100000) identity double
  foreachE (toUnfoldable (topologicalSort graph)) logShow
