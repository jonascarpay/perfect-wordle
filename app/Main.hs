{-# LANGUAGE LambdaCase #-}

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
  interactive dict smartest (Just $ parseWord "roate")

interactive :: Dictionary -> Solver -> Maybe Word' -> IO ()
interactive dict solver mfirst = go (fromMaybe (solver dict mempty) mfirst) mempty
  where
    parseChar :: Char -> Maybe LetterResponse
    parseChar 'G' = pure Green
    parseChar 'g' = pure Green
    parseChar 'Y' = pure Yellow
    parseChar 'y' = pure Yellow
    parseChar '.' = pure Grey
    parseChar _ = Nothing
    ask :: Word' -> IO (V5 LetterResponse)
    ask w = do
      putStrLn $ showWord w <> "?\t\t'.' for grey, 'y'/'Y' for yellow, 'g'/'G' for green"
      getLine >>= \case
        [a, b, c, d, e] | Just v <- traverse parseChar (V5 a b c d e) -> pure v
        _ -> putStrLn "Parse error" >> ask w
    go guess k = do
      resp <- ask guess
      if resp == V5 Green Green Green Green Green
        then pure ()
        else
          let k' = k <> fromResponse guess resp
           in go (solver dict k') k'

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
  where
    average :: [Int] -> Double
    average xs = fromIntegral (sum xs) / fromIntegral (length xs)

loadWordList :: FilePath -> IO [Word']
loadWordList fp = fmap parseWord . lines <$> readFile fp
