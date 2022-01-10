import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Ord
import Lib
import Solvers
import qualified System.Exit as Sys

main :: IO ()
main = do
  valid <- loadWordList "./valid.txt"
  answers <- loadWordList "./words.txt"
  let dict = Dictionary valid answers
  evalSolver smartest dict (parseWord "raise") (fromJust $ parseWord "match")

evalSolver :: Solver -> Dictionary -> Maybe Word' -> Word' -> IO ()
evalSolver solver dict mfirst ans = do
  first <- case mfirst of
    Nothing ->
      let g = solver dict mempty
       in g <$ putStrLn ("First guess: " <> showWord g)
    Just g -> g <$ putStrLn ("First guess (forced): " <> showWord g)
  putStrLn $ unwords $ fmap showWord $ playPure solver dict ans (Just first)
  let g = flip pmap (answerWords dict) $ \ans ->
        let guesses = playPure solver dict ans (Just first)
         in seq guesses (length guesses, guesses)
      (n, ws) = maximumBy (comparing fst) g
      av = average (fmap fst g)
  putStrLn $ "Average: " <> show av
  putStrLn $ "Worst (" <> show n <> "): " <> unwords (fmap showWord ws)

average :: [Int] -> Double
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

parseWordIO :: String -> IO Word'
parseWordIO w = maybe (Sys.die $ "Couldn't parse " <> show w) pure $ parseWord w

loadWordList :: FilePath -> IO [Word']
loadWordList fp = readFile fp >>= traverse parseWordIO . lines
