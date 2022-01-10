import Control.Monad
import Data.Foldable
import Data.Ord
import Lib
import Solvers
import qualified System.Exit as Sys

main :: IO ()
main = do
  words <- loadWordList "./words.txt"
  printLengthStats words simple

printLengthStats :: [Word'] -> Solver -> IO ()
printLengthStats words s = do
  putStrLn $ "Average: " <> show av
  putStrLn $ "Worst (" <> show n <> "): " <> unwords (fmap showWord ws)
  where
    (n, ws) = maximumBy (comparing fst) g
    av = average (fmap fst g)
    g = flip fmap words $ \ans ->
      let guesses = playPure simple words ans (parseWord "raise")
       in (length guesses, guesses)

    average :: [Int] -> Double
    average xs = fromIntegral (sum xs) / fromIntegral (length xs)

parseWordIO :: String -> IO Word'
parseWordIO w = maybe (Sys.die $ "Couldn't parse " <> show w) pure $ parseWord w

loadWordList :: FilePath -> IO [Word']
loadWordList fp = readFile fp >>= traverse parseWordIO . lines
