{-# LANGUAGE Strict #-}

module Solvers
  ( sieve,
    smart,
    smartest,
  )
where

import Control.Parallel.Strategies
import Data.Foldable
import Data.Ord
import Lib

{-# INLINE minimumOn #-}
minimumOn :: (a -> Int) -> [a] -> a
minimumOn f = snd . minimumBy (comparing fst) . pmap (\x -> let n = f x in seq n (n, x))

{-# INLINE sieveLike #-}
sieveLike :: (Dictionary -> [Word'] -> Knowledge -> [Word']) -> Solver
sieveLike f dict@(Dictionary _ answers) know =
  case filter (fits know) answers of
    [] -> error "No possible answers!"
    [a] -> a
    [a, _] -> a
    answers' -> flip minimumOn (f dict answers' know) $ \guess ->
      sum [count (fits (know <> rate act guess)) answers' | act <- answers']

sieve :: Solver
sieve = sieveLike $ \_ remaining _ -> remaining

smart :: Solver
smart = sieveLike $ \(Dictionary _ answers) _ _ -> answers

smartest :: Solver
smartest = sieveLike $ \(Dictionary valid _) a' _ -> a' <> valid -- Sticking the possible answers at the front makes it so we prefer words them as guesses

{-# INLINE count #-}
count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

{-# INLINE pmap #-}
pmap :: (a -> b) -> [a] -> [b]
pmap = parMap rpar
