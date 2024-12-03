module Main where

import Aoc (runner)
import Control.Applicative (Alternative (..))
import Control.Monad (guard, when)
import Control.Monad.RWS (
  MonadReader (ask),
  MonadState (get, put),
  MonadWriter (tell),
  RWS,
  execRWS,
 )
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Char (isDigit)
import Data.Monoid (Sum (Sum, getSum))
import Text.Read (readMaybe)

main :: IO ()
main = runner lines (solver True) (solver False)
 where
  solver p1 = runApp p1 . traverse findMuls

{- | The App is a Alternative RWS,
Reading part, Writing the Sum and keeping activation state
-}
type App = MaybeT (RWS Bool (Sum Integer) Bool)

runApp :: Bool -> App a -> Integer
runApp part1 app = getSum . snd $ execRWS (runMaybeT app) (not part1) True

findMuls :: String -> App ()
findMuls [] = pure ()
findMuls ('d' : 'o' : '(' : ')' : rest) = putIfActive True >> findMuls rest
findMuls ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : rest) = putIfActive False >> findMuls rest
findMuls ('m' : 'u' : 'l' : '(' : ss) = happyPath <|> findMuls ss
 where
  happyPath = do
    mulIsActive <- get
    guard mulIsActive
    -- We are in a MonadFail so we handle the pattern match
    (x', (',' : rest)) <- pure $ break (not . isDigit) ss
    (y', (')' : rest')) <- pure $ break (not . isDigit) rest
    x <- MaybeT . return $ readMaybe x'
    y <- MaybeT . return $ readMaybe y'
    tell $ Sum (x * y)
    findMuls rest'
findMuls (_ : rest) = findMuls rest

putIfActive :: Bool -> App ()
putIfActive s = do
  shouldAct <- ask
  when shouldAct $ put s
