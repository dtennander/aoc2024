module Main where

import Aoc (runner)
import Control.Monad (guard, join)

main :: IO ()
main = runner lines p1 p2

type Input = [[Char]]

p1 :: Input -> Int
p1 input = length . filter (== "XMAS") $ pathsAsStrings input paths
 where
  width = length input
  height = length (input !! 0)
  paths = join [linePaths (x, y) | x <- [0 .. width], y <- [0 .. height]]

p2 :: Input -> Int
p2 input = length . filter ((== 2) . (length . filter (== "MAS"))) $ fmap (pathsAsStrings input) paths
 where
  width = length input
  height = length (input !! 0)
  paths = [xPaths (x, y) | x <- [0 .. width], y <- [0 .. height]]

type Point = (Int, Int)
type Path = [Point]

xPaths :: Point -> [Path]
xPaths (x, y) = do
  d1 <- [-1, 1]
  d2 <- [-1, 1]
  return $ path (x - d1, y - d2) (d1, d2) 3

linePaths :: Point -> [Path]
linePaths (x, y) = do
  d1 <- [-1 .. 1]
  d2 <- [-1 .. 1]
  guard (d1 /= 0 || d2 /= 0)
  return $ path (x, y) (d1, d2) 4

path :: Point -> (Int, Int) -> Int -> Path
path (x, y) (d1, d2) l = do
  i <- [0 .. (l - 1)]
  let x' = x + i * d1
  let y' = y + i * d2
  return $ (x', y')

pathsAsStrings :: [String] -> [Path] -> [String]
pathsAsStrings input = fmap withValue . filter (all inside)
 where
  width = length input
  height = length (input !! 0)
  inside (x, y) = 0 <= x && x < width && 0 <= y && y < height
  withValue ps = do
    (x, y) <- ps
    return $ (input !! x) !! y
