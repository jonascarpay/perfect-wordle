module Solvers
  ( simple,
  )
where

import Control.Parallel.Strategies
import Data.Foldable
import Data.Ord
import Lib

minimumOn :: (a -> Int) -> [a] -> a
minimumOn f = snd . minimumBy (comparing fst) . parMap rpar (\x -> let n = f x in seq n (n, x))

simple :: Solver
simple (Dictionary _ answers) know = flip minimumOn candidates $ \guess ->
  let n = sum $ do
        act <- candidates
        let know' = rate act guess
        pure $ length $ filter (fits know') candidates
   in n
  where
    candidates = filter (fits know) answers