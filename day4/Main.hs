module Main where

import Aoc (runner)
import Control.Monad (guard)

main :: IO ()
main = runner lines p1 p2

type Input = [[Char]]
type Point = (Int, Int)
type Direction = (Int, Int)

p1 :: Input -> Int
p1 input = length . filter (== "XMAS") $ paths
 where
  width = length input
  height = length (input !! 0)
  paths = do
    x <- [0 .. width]
    y <- [0 .. height]
    d1 <- [-1 .. 1]
    d2 <- [-1 .. 1]
    guard (d1 /= 0 || d2 /= 0)
    let p = path (x, y) (d1, d2) 4 input
    guard (length p == 4)
    return p

p2 :: Input -> Int
p2 input = length . filter (\ps -> 2 == (length . filter (== "MAS") $ ps)) $ paths
 where
  width = length input
  height = length (input !! 0)
  xPaths (x, y) = do
    d1 <- [-1, 1]
    d2 <- [-1, 1]
    let p = path (x - d1, y - d2) (d1, d2) 3 input
    guard (length p == 3)
    return p
  paths = do
    x <- [0 .. width]
    y <- [0 .. height]
    return $ xPaths (x, y)

path :: Point -> Direction -> Int -> [[a]] -> [a]
path (x, y) (d1, d2) l input = do
  let width = length input
  let height = length (input !! 0)
  i <- [0 .. (l - 1)]
  let x' = x + i * d1
  let y' = y + i * d2
  guard (0 <= x')
  guard (0 <= y')
  guard (x' < width)
  guard (y' < height)
  return $ (input !! x') !! y'
