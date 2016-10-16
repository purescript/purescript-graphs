module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Graph (Graph(..), topSort)
import Data.List (fromFoldable, range)
import Data.Monoid (mempty)
import Data.Map as M
import Data.Foldable (traverse_)
import Data.Tuple (Tuple(..))

main :: Eff (console :: CONSOLE) Unit
main = do
  let sorted = topSort graph
      graph = Graph { vertices: M.fromList (map double (range 1 1000)) }
      double x = Tuple x { value: x, out: fromFoldable (if x <= 500 then [x * 2] else []) }
  traverse_ logShow sorted
