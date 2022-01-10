import Control.Monad
import Data.Foldable
import Data.Ord
import Lib
import Solvers
import qualified System.Exit as Sys

main :: IO ()
main = do
  valid <- loadWordList "./valid.txt"
  answers <- loadWordList "./words.txt"
  printLengthStats simple (Dictionary valid answers)

printLengthStats :: Solver -> Dictionary -> IO ()
printLengthStats solver dict = do
  putStrLn $ "Average: " <> show av
  putStrLn $ "Worst (" <> show n <> "): " <> unwords (fmap showWord ws)
  where
    g = flip fmap (answerWords dict) $ \ans ->
      let guesses = playPure simple dict ans (parseWord "raise")
       in (length guesses, guesses)
    (n, ws) = maximumBy (comparing fst) g
    av = average (fmap fst g)

    average :: [Int] -> Double
    average xs = fromIntegral (sum xs) / fromIntegral (length xs)

parseWordIO :: String -> IO Word'
parseWordIO w = maybe (Sys.die $ "Couldn't parse " <> show w) pure $ parseWord w

loadWordList :: FilePath -> IO [Word']
loadWordList fp = readFile fp >>= traverse parseWordIO . lines
