module IntricateCode where

import Data.List (nub, intercalate)
import Text.Printf (printf)
import Data.Function (fix, on)
import System.Random (StdGen, random, newStdGen, split)
import Control.Monad.Random (MonadRandom)

type Stack = [Int]

getUniqueNumber :: StdGen -> Int -> IO Int
getUniqueNumber gen threshold = do
  seed <- liftIO $ random gen
  let n = mod seed threshold
  if n `elem` [0 .. threshold - 1]
  then liftIO $ getUniqueNumber gen threshold
  else liftIO $ return n

generateStack :: MonadRandom m => Int -> m Stack
generateStack n = mapM (\_ -> liftIO $ getUniqueNumber rnd 10) [1..n]
  where
    rnd = newStdGen

findSequences :: Stack -> [[Int]]
findSequences stack = fst $ fix $ go
  where
    go :: [[Int]] -> Stack -> ([[Int]], Stack)
    go accs [] = (accs, [])
    go accs (x:xs) = go (accs ++ [[x]]) xs ++
                      go (accs ++ [x:ys]) zs
      where
        (ys, zs) = splitAt (length accs) xs

printSequences :: [[Int]] -> String
printSequences seqs = intercalate "\n" $ map (printf "%s") seqs

main :: IO ()
main = do
  n <- liftIO $ getLine
  stack <- liftIO $ generateStack (read n :: Int)
  let sequences = findSequences stack
  putStrLn $ printSequences sequences