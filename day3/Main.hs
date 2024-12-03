{-# LANGUAGE FlexibleContexts #-}

module Main where

import Aoc (runner)
import Control.Arrow (first)
import Data.Char (isDigit)
import Text.Read (readMaybe)

main :: IO ()
main = runner parse p1 p2

type File = [Program]
type Program = String

parse :: String -> File
parse = lines

p1 :: File -> Integer
p1 = sum . fst . foldl' step ([], True)
 where
  step (sums, isOn) = first (<> sums) . findMuls False isOn

findMuls :: Bool -> Bool -> String -> ([Integer], Bool)
findMuls globalOn True ('m' : 'u' : 'l' : '(' : ss) =
  let
    (nextProduct, r) = maybe (0, ss) id $ do
      -- Failed pattern matching results in None
      (x', (',' : rest)) <- pure $ break (not . isDigit) ss
      (y', (')' : rest')) <- pure $ break (not . isDigit) rest
      -- Let's be safe while already are in a Maybe
      x <- readMaybe x'
      y <- readMaybe y'
      return $ (x * y, rest')
    (t, endResult) = findMuls globalOn True r
   in
    (nextProduct : t, endResult)
findMuls _ isOn [] = ([], isOn)
findMuls True _ ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : rest) = findMuls True False rest
findMuls True _ ('d' : 'o' : '(' : ')' : rest) = findMuls True True rest
findMuls globalOn isOn (_ : rest) = findMuls globalOn isOn rest

p2 :: File -> Integer
p2 = sum . fst . foldl' step ([], True)
 where
  step (sums, isOn) = first (<> sums) . findMuls True isOn
