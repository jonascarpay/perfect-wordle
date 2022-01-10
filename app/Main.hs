import Control.Monad
import Lib

main :: IO ()
main = do
  words <- loadWordList "./words.txt"

  print $ evaluate words

evaluate words = average $ fmap (\ans -> length $ playPure words ans (parseWord "raise")) words

average :: [Int] -> Double
average xs = fromIntegral (sum xs) / fromIntegral (length xs)
