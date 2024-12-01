module Main where

import Aoc (runner)
import Control.Monad (liftM)
import Data.List (sort, transpose)

main :: IO ()
main = runner readLines part1 part2

-- | readLines parses the input as the two rows of numbers.
readLines :: String -> [[Integer]]
readLines = transpose . fmap (fmap read . words) . lines

part1 :: [[Integer]] -> Maybe Integer
part1 = liftM sum . traverse absPairs . transpose . fmap sort
 where
  absPairs [a, b] = Just $ abs (a - b)
  absPairs _ = Nothing

part2 :: [[Integer]] -> Maybe Integer
part2 = liftM sum . multiplyOccurences . fmap countElements
 where
  countElements = foldl' count [] . sort

  count (h@(n, times) : rest) x
    | x == n = (n, times + 1) : rest
    | otherwise = (x, 1) : h : rest
  count [] x = [(x, 1)]

  multiplyOccurences [as, bs] = Just $ do
    (a, x) <- as
    case lookup a bs of
      Just y -> return (a * x * y)
      Nothing -> return 0
  multiplyOccurences _ = Nothing
