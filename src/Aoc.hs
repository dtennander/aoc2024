module Aoc where

import System.Environment (getArgs)

runner :: (Show b, Show c, Failable v a) => (String -> a) -> (v -> b) -> (v -> c) -> IO ()
runner parser p1 p2 = do
  args <- getArgs
  let program = if "-p2" `elem` args then show . p2 else show . p1
  interact (either id program . extract . parser) >> putStrLn ""

class Failable v a where
  extract :: a -> Either String v

instance Failable v (Either String v) where
  extract = id

instance Failable v (Maybe v) where
  extract = maybe (Left "Failed to parse Input!") Right

instance Failable v v where
  extract = Right
