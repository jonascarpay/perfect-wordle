{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib where

import Control.Applicative
import Control.Monad.Writer
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

data Slot = IsNot (Set Letter) | Is Letter

instance Semigroup Slot where
  {-# INLINE (<>) #-}
  Is a <> Is b = if a == b then Is a else error "impossible"
  Is a <> IsNot _ = Is a
  IsNot _ <> Is b = Is b
  IsNot a <> IsNot b = IsNot (a <> b)

instance Monoid Slot where
  {-# INLINE mempty #-}
  mempty = IsNot mempty

data Knowledge = Knowledge
  { greys :: Set Letter,
    yellows :: Set Letter,
    slots :: V5 Slot
  }

instance Semigroup Knowledge where
  {-# INLINE (<>) #-}
  (Knowledge ga ya sa) <> (Knowledge gb yb sb) = Knowledge (ga <> gb) (ya <> yb) (sa <> sb)

instance Monoid Knowledge where
  {-# INLINE mempty #-}
  mempty = Knowledge mempty mempty mempty

type Solver = Dictionary -> Knowledge -> Word'

{-# INLINE fits #-}
fits :: Knowledge -> Word' -> Bool
fits (Knowledge greys yellows slots) word =
  all (flip Set.notMember greys) word
    && all (flip elem word) yellows
    && and (liftA2 fitSlot slots word)
  where
    fitSlot :: Slot -> Letter -> Bool
    fitSlot (IsNot ins) c = Set.notMember c ins
    fitSlot (Is g) c = g == c

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
      | s == g = Is s
      | Set.member g sletters = IsNot (Set.singleton g)
      | otherwise = IsNot mempty

data LetterResponse = Grey | Yellow | Green
  deriving (Eq, Show)

fromResponse :: Word' -> V5 LetterResponse -> Knowledge
fromResponse w r =
  let (v, (gr, yl)) = runWriter (sequence $ liftA2 f w r)
   in Knowledge gr yl v
  where
    f :: Letter -> LetterResponse -> Writer (Set Letter, Set Letter) Slot
    f l Grey = IsNot mempty <$ tell (Set.singleton l, mempty)
    f l Yellow = IsNot (Set.singleton l) <$ tell (mempty, Set.singleton l)
    f l Green = Is l <$ tell (mempty, mempty)

fromChar :: Char -> Maybe Letter
fromChar c
  | isAsciiLower c = Just $ toEnum $ ord c - ord 'a'
  | isAsciiUpper c = Just $ toEnum $ ord c - ord 'A'
  | otherwise = Nothing

toChar :: Letter -> Char
toChar = chr . (+ ord 'A') . fromEnum

parseWord :: String -> Word'
parseWord [a, b, c, d, e] | Just v <- traverse fromChar (V5 a b c d e) = v
parseWord str = error $ "Couldn't parse " <> str

showWord :: Word' -> String
showWord = foldMap show

playPure :: Solver -> Dictionary -> Word' -> Maybe Word' -> [Word']
playPure solver dict answer mfirst =
  case mfirst of
    Nothing -> go mempty
    Just f -> f : go (rate answer f)
  where
    go k =
      let guess = solver dict k
       in guess : if guess == answer then [] else go (k <> rate answer guess)
