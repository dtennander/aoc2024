module Main where

import Aoc (runner)
import Control.Monad (guard)
import Data.List (elemIndex, (!?))
import Data.List.Split (split, splitOn)
import Text.Read (readMaybe)

main :: IO ()
main = runner parse p1 p2

type Rule = (Int, Int)
type Update = [Int]
data Input = Input
  { rules :: [Rule]
  , updates :: [Update]
  }

parse :: String -> Maybe Input
parse s =
  Input <$> rules <*> updates
 where
  rules = traverse asRule =<< (lines <$> first)
  updates = traverse (traverse readMaybe . splitOn ",") =<< lines <$> second
  first = sections !? 0
  second = sections !? 1
  sections = splitOn "\n\n" s
  asRule l = case splitOn "|" l of
    [a, b] -> Just (read a, read b)
    _ -> Nothing

p1 :: Input -> Int
p1 (Input{rules, updates}) = sum . fmap takeMid . filter (follows rules) $ updates

takeMid :: [a] -> a
takeMid xs = xs !! ((length xs) `div` 2)

follows :: [Rule] -> Update -> Bool
follows rs u = all (matches) rs
 where
  matches r = maybe True id $ do
    (a, b) <- getIndexes r u
    return (a < b)

getIndexes :: (Eq a) => (a, a) -> [a] -> Maybe (Int, Int)
getIndexes (l, r) xs = do
  a <- elemIndex l xs
  b <- elemIndex r xs
  return (a, b)

p2 :: Input -> Int
p2 (Input{rules, updates}) = sum . fmap (takeMid . until (follows rules) update) . filter (not . follows rules) $ updates
 where
  update u = foldl' applyRule u rules

applyRule :: Update -> Rule -> Update
applyRule u r = maybe u id $ do
  (a, b) <- getIndexes r u
  guard (a > b)
  (start, (r' : tail')) <- return $ splitAt b u
  return $ setAt a r' (start <> tail')

setAt :: Int -> a -> [a] -> [a]
setAt i v xs = start <> (v : end)
 where
  (start, end) = splitAt i xs
