{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Lib where

import Control.Applicative
import Control.Parallel.Strategies
import Data.Char (chr, isAsciiLower, isAsciiUpper, ord)
import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as Set

data Dictionary = Dictionary
  { validWords :: [Word'],
    answerWords :: [Word']
  }

data V5 a = V5 a a a a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Semigroup a => Semigroup (V5 a) where
  (<>) = liftA2 (<>)
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (V5 a) where
  mempty = pure mempty
  {-# INLINE mempty #-}

type Word' = V5 Letter

instance Applicative V5 where
  pure a = V5 a a a a a
  V5 fa fb fc fd fe <*> V5 a b c d e = V5 (fa a) (fb b) (fc c) (fd d) (fe e)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

data Letter = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Show, Eq, Ord, Enum)

data Slot = Yellow (Set Letter) | Green Letter

instance Semigroup Slot where
  Green a <> Green b = if a == b then Green a else error "impossible"
  Green a <> Yellow _ = Green a
  Yellow _ <> Green b = Green b
  Yellow a <> Yellow b = Yellow (a <> b)

instance Monoid Slot where mempty = Yellow mempty

data Knowledge = Knowledge
  { greys :: Set Letter,
    yellows :: Set Letter,
    slots :: V5 Slot
  }

instance Semigroup Knowledge where
  (Knowledge ga ya sa) <> (Knowledge gb yb sb) = Knowledge (ga <> gb) (ya <> yb) (sa <> sb)
  {-# INLINE (<>) #-}

instance Monoid Knowledge where
  mempty = Knowledge mempty mempty mempty
  {-# INLINE mempty #-}

type Solver = Dictionary -> Knowledge -> Word'

{-# INLINE fits #-}
fits :: Knowledge -> Word' -> Bool
fits (Knowledge greys yellows slots) word =
  all (flip Set.notMember greys) word
    && all (flip elem word) yellows
    && and (liftA2 fitSlot slots word)
  where
    {-# INLINE fitSlot #-}
    fitSlot :: Slot -> Letter -> Bool
    fitSlot (Yellow ins) c = Set.notMember c ins
    fitSlot (Green g) c = g == c

{-# INLINE rate #-}
rate :: Word' -> Word' -> Knowledge
rate solution = \guess ->
  Knowledge
    (Set.fromList $ filter (flip Set.notMember sletters) $ toList guess)
    (Set.fromList $ filter (flip Set.member sletters) $ toList guess)
    (liftA2 slot solution guess)
  where
    sletters = Set.fromList $ toList solution
    slot s g
      | s == g = Green s
      | Set.member g sletters = Yellow (Set.singleton g)
      | otherwise = Yellow mempty

fromChar :: Char -> Maybe Letter
fromChar c
  | isAsciiLower c = Just $ toEnum $ ord c - ord 'a'
  | isAsciiUpper c = Just $ toEnum $ ord c - ord 'A'
  | otherwise = Nothing

toChar :: Letter -> Char
toChar = chr . (+ ord 'A') . fromEnum

parseWord :: String -> Maybe Word'
parseWord [a, b, c, d, e] = traverse fromChar $ V5 a b c d e
parseWord _ = Nothing

showWord :: Word' -> String
showWord = show . foldMap show

playPure :: Solver -> Dictionary -> Word' -> Maybe Word' -> [Word']
playPure solver dict answer mfirst =
  case mfirst of
    Nothing -> go mempty
    Just f -> f : go (rate answer f)
  where
    go k =
      let guess = solver dict k
       in guess : if guess == answer then [] else go (k <> rate answer guess)

pmap :: (a -> b) -> [a] -> [b]
pmap = parMap rpar
