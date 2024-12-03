module Main where

import Aoc (runner)

main :: IO ()
main = runner parseReports p1 p2
 where
  p1 = solve False
  p2 = solve True

type Report = [Level]
type Level = Int

parseReports :: String -> [Report]
parseReports = fmap parseLine . lines
 where
  parseLine = fmap read . words

solve :: Bool -> [Report] -> Int
solve p2 = sum . fmap (fromEnum . isSafe p2)

isSafe :: Bool -> Report -> Bool
isSafe p2 (x : y : rest) = failed $ foldl' checkEach initState (y : rest)
 where
  initState = FoldState{failed = False, lastLevel = x, haveDroppedOne = False}
  checkEach s n
    | 1 <= diff && diff <= 3 = s{lastLevel = n}
    | p2 && not (haveDroppedOne s) = s{haveDroppedOne = True}
    | otherwise = s{failed = True}
   where
    diff = direction * ((lastLevel s) - n)
  direction = max (-1) . min 1 $ (x - y)
isSafe _ _ = False

data FoldState = FoldState
  { failed :: Bool
  , lastLevel :: Level
  , haveDroppedOne :: Bool
  }
