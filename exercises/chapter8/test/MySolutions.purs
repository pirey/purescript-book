module Test.MySolutions where

import Prelude

import Control.Monad.ST (for, run)
import Control.Monad.ST.Ref (modify, new, read, write)
import Data.Array (foldM, head, nub, sort, tail)
import Data.Int (even, toNumber)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Exception (throw)

-- Note to reader: Add your solutions to this file
third :: forall a. Array a -> Maybe a
third arr = do
  skip1st <- tail arr
  skip2nd <- tail skip1st
  head skip2nd

possibleSums :: Array Int -> Array Int
possibleSums xs = nub $ sort $ foldM (\acc i -> [ acc, acc + i ]) 0 xs

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM f (x : xs) = do
  b <- f x
  xs' <- filterM f xs
  pure if b then x : xs' else xs'

exceptionDivide :: Int -> Int -> Effect Int
exceptionDivide _ 0 = throw "div zero"
exceptionDivide x y = pure $ x / y

estimatePi :: Int -> Number
estimatePi n =
  run do
    accRef <- new 0.0
    for 1 (n + 1) \k ->
      let
        sign = if even k then -1.0 else 1.0
      in
        modify (\acc -> acc + sign / (2.0 * toNumber k - 1.0)) accRef
    final <- read accRef
    pure $ final * 4.0

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n =
  run
    ( do
        x <- new 0
        y <- new 1
        for 2 n \_ -> do
          x' <- read x
          y' <- read y
          _ <- write (x' + y') y
          write y' x
        x' <- read x
        y' <- read y
        pure $ x' + y'
    )
