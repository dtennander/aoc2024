module Main where

import Aoc (runner)
import Control.Applicative (Alternative (..))
import Control.Monad (guard, when)
import Data.Char (isDigit)
import Text.Read (readMaybe)

main :: IO ()
main = runner lines (solver True) (solver False)

solver :: Bool -> [String] -> Integer
solver p1 = sum . getLog (not p1) True . traverse findMuls

findMuls :: String -> RWSM Bool Integer Bool ()
findMuls [] = pure ()
findMuls ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : rest) = putIfOk False >> findMuls rest
findMuls ('d' : 'o' : '(' : ')' : rest) = putIfOk True >> findMuls rest
findMuls ('m' : 'u' : 'l' : '(' : ss) = happyPath <|> default'
 where
  default' = findMuls ss
  happyPath = do
    get >>= guard
    (x', (',' : rest)) <- pure $ break (not . isDigit) ss
    (y', (')' : rest')) <- pure $ break (not . isDigit) rest
    x <- lift $ readMaybe x'
    y <- lift $ readMaybe y'
    write' (x * y)
    findMuls rest'
findMuls (_ : rest) = findMuls rest

putIfOk :: s -> RWSM Bool w s ()
putIfOk s = do
  shouldAct <- read'
  when shouldAct $ put s

newtype RWSM r w s a = RWS {run :: r -> s -> ([w], s, Maybe a)}

getLog :: r -> s -> RWSM r w s a -> [w]
getLog r s rws = let (ws, _, _) = run rws r s in ws

get :: RWSM r w s s
get = RWS $ \_ s -> ([], s, Just s)

read' :: RWSM r w s r
read' = RWS $ \r s -> ([], s, Just r)

put :: s -> RWSM r w s ()
put s = RWS $ \_ _ -> ([], s, Just ())

write' :: w -> RWSM r w s ()
write' w = RWS $ \_ s -> ([w], s, Just ())

lift :: Maybe a -> RWSM r w s a
lift a = RWS $ \_ s -> ([], s, a)

instance Functor (RWSM r w s) where
  fmap f (RWS (rws)) = RWS $ \r s ->
    case rws r s of
      (w, s', Just a) -> (w, s', Just $ f a)
      (w, s', Nothing) -> (w, s', Nothing)

instance Applicative (RWSM r w s) where
  pure a = RWS $ (\_ s -> ([], s, Just a))
  RWS (rwsF) <*> RWS (rwsA) = RWS $ \r s ->
    case rwsA r s of
      (w, s', Nothing) -> (w, s', Nothing)
      (w, s', Just a) -> case rwsF r s' of
        (w', s'', Just f) -> (w <> w', s'', Just $ f a)
        (w', s'', Nothing) -> (w <> w', s'', Nothing)

instance Monad (RWSM r w s) where
  RWS (rwsA) >>= f = RWS $ \r s ->
    case rwsA r s of
      (w, s', Nothing) -> (w, s', Nothing)
      (w, s', Just a) ->
        let (w', s'', b) = (run $ f a) r s'
         in (w <> w', s'', b)

instance MonadFail (RWSM r w s) where
  fail _ = RWS $ \_ s -> ([], s, Nothing)

instance Alternative (RWSM r w s) where
  empty = RWS $ \_ s -> ([], s, Nothing)
  (RWS l) <|> (RWS ri) = RWS $ \r s ->
    case l r s of
      l'@(_, _, Just _) -> l'
      (_, _, Nothing) -> ri r s
