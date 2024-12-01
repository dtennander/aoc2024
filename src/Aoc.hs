module Aoc where

import System.Environment (getArgs)

runner :: (Show b, Show c) => (String -> a) -> (a -> b) -> (a -> c) -> IO ()
runner parser p1 p2 = do
  args <- getArgs
  let program = if "-p2" `elem` args then show . p2 else show . p1
  interact (program . parser)
