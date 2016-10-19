module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Foldable (traverse_)
import Data.Graph (unfoldGraph, topologicalSort)
import Data.List (range)

main :: Eff (console :: CONSOLE) Unit
main = do
  let double x | x * 3 < 1000 = [x * 2, x * 3]
               | otherwise    = []
      graph = unfoldGraph (range 1 1000) id double
  traverse_ logShow (topologicalSort graph)
