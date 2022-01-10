{-# LANGUAGE Strict #-}

module Solvers
  ( sieve,
    smart,
    smartest,
  )
where

import Data.Foldable
import Data.Ord
import qualified Data.Set as Set
import Lib

minimumOn :: (a -> Int) -> [a] -> a
minimumOn f = snd . minimumBy (comparing fst) . pmap (\x -> let n = f x in seq n (n, x))

{-# INLINE sieveLike #-}
sieveLike :: (Dictionary -> [Word'] -> Knowledge -> [Word']) -> Solver
sieveLike f dict@(Dictionary _ answers) know =
  case filter (fits know) answers of
    [a] -> a
    [a, _] -> a
    answers' -> flip minimumOn (f dict answers' know) $ \guess ->
      sum [count (fits (know <> rate act guess)) answers' | act <- answers']

sieve :: Solver
sieve = sieveLike $ \_ remaining _ -> remaining

smart :: Solver
smart = sieveLike $ \(Dictionary _ answers) _ _ -> answers

smartFast :: Solver
smartFast = sieveLike $ \(Dictionary _ answers) _ (Knowledge g _ _) -> filter ((>= 2) . count (flip Set.notMember g) . toList) answers

smartest :: Solver
smartest = sieveLike $ \(Dictionary valid _) _ (Knowledge g _ _) -> valid

{-# INLINE count #-}
count :: (a -> Bool) -> [a] -> Int
count p = length . filter p
