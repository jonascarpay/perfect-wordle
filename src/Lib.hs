{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Lib
  ( playPure,
    parseWordIO,
    showWord,
    parseWord,
    loadWordList,
  )
where

import Control.Applicative
import Control.Monad
import Control.Parallel.Strategies
import Data.Char (chr, isAsciiLower, isAsciiUpper, ord)
import Data.Foldable
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified System.Exit as Sys

data V5 a = V5 a a a a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Semigroup a => Semigroup (V5 a) where (<>) = liftA2 (<>)

instance Monoid a => Monoid (V5 a) where mempty = pure mempty

type Word' = V5 Letter

instance Applicative V5 where
  pure a = V5 a a a a a
  V5 fa fb fc fd fe <*> V5 a b c d e = V5 (fa a) (fb b) (fc c) (fd d) (fe e)

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

instance Semigroup Knowledge where (Knowledge ga ya sa) <> (Knowledge gb yb sb) = Knowledge (ga <> gb) (ya <> yb) (sa <> sb)

instance Monoid Knowledge where mempty = Knowledge mempty mempty mempty

fits :: Knowledge -> Word' -> Bool
fits (Knowledge greys yellows slots) word =
  and
    [ all (flip Set.notMember greys) word,
      all (flip elem word) yellows,
      and $ liftA2 fitSlot slots word
    ]

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

fitSlot :: Slot -> Letter -> Bool
fitSlot (Yellow ins) c = Set.notMember c ins
fitSlot (Green g) c = g == c

fromChar :: Char -> Maybe Letter
fromChar c
  | isAsciiLower c = Just $ toEnum $ ord c - ord 'a'
  | isAsciiUpper c = Just $ toEnum $ ord c - ord 'A'
  | otherwise = Nothing

minimumOn :: (a -> Int) -> [a] -> a
minimumOn f = snd . minimumBy (comparing fst) . parMap rpar (\x -> let n = f x in seq n (n, x))

findGuess :: [Word'] -> Knowledge -> Word'
findGuess candidates know = flip minimumOn candidates' $ \guess ->
  let n = sum $ do
        act <- candidates'
        let know' = rate act guess
        pure $ length $ filter (fits know') candidates'
   in n
  where
    candidates' = filter (fits know) candidates

toChar :: Letter -> Char
toChar = chr . (+ ord 'A') . fromEnum

parseWord :: String -> Maybe Word'
parseWord [a, b, c, d, e] = traverse fromChar $ V5 a b c d e
parseWord _ = Nothing

parseWordIO :: String -> IO Word'
parseWordIO w = maybe (Sys.die $ "Couldn't parse " <> show w) pure $ parseWord w

loadWordList :: FilePath -> IO [Word']
loadWordList fp = readFile fp >>= traverse parseWordIO . lines

showWord :: Word' -> String
showWord = show . foldMap show

playPure :: [Word'] -> Word' -> Maybe Word' -> [Word']
playPure words answer mfirst =
  case mfirst of
    Nothing -> go mempty
    Just f -> f : go (rate answer f)
  where
    go k =
      let guess = findGuess words k
       in guess : if guess == answer then [] else go (k <> rate answer guess)
