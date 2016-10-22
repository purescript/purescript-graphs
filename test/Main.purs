module Test.Main where

import Prelude
import Control.Monad.Eff (foreachE, Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Graph (unfoldGraph, topologicalSort)
import Data.List (toUnfoldable, range)

main :: Eff (console :: CONSOLE) Unit
main = do
  let double x | x * 2 < 100000 = [x * 2]
               | otherwise      = []
      graph = unfoldGraph (range 1 100000) id double
  foreachE (toUnfoldable (topologicalSort graph)) logShow
