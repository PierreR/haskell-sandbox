{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}
-- | Main entry point to the application.
module Main where

import           Control.Monad              (guard)
import           Control.Monad.Trans.Either
import           Data.Char                  (digitToInt)
import           Data.Either.Combinators
import           Data.List                  (sort)
import qualified Data.Yaml                  as Y

-- I import qualified so that it's clear which
-- functions are from the parsec library:
import qualified Text.Parsec                as Parsec

-- I am the error message infix operator, used later:
import           Text.Parsec                ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import           Control.Applicative

-- Get the Identity monad from here:
import           Control.Monad.Identity     (Identity)

-- alias Parsec.parse for more concise usage in my examples:
parse rule text = Parsec.parse rule "(source)" text

myParser :: Parsec.Parsec String () (String,String)
myParser = do
    letters <- Parsec.many1 Parsec.letter
    Parsec.spaces
    digits <- Parsec.many1 Parsec.digit
    return (letters,digits)

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ smaller
    where smaller = filter (< x) xs
          larger = filter (>=x) xs

--digitize :: Integer -> [Integer]
digitize i = map digitToInt (sort $ show i)

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop (n+1)  xs)
    where n = (length xs) `div` 2

halve' xs = splitAt (length xs `div` 2)

safeTail :: [a] -> [a]
safeTail xs = if null xs then [] else tail xs

trues a b = if a then if b then True else False else False

products :: Int -> [Int]
products n = [x | x <- [1 ..n], n `mod` x == 0]

products' n = do
    x <- [1..n]
    guard $ n `mod` x == 0
    return x

perfects n = [x | x <- [1..n], isPerfect x]
isPerfect num = sum (products num) == num

-- this is equal to [(x,y) | x <- [1,2,3], y <- [4,5,6]]
comprehension = concat [ [(x,y) | y <- [4,5,6]] | x <- [1,2,3]]

isPrime :: Int -> Bool
isPrime n = products n == [1,n]

primes = [ x | x <- [1..], isPrime x]

riffle xs ys = concat [[x,y] | (x,y) <- xs `zip` ys]

find' k t = [v | (k', v) <- t, k == k']

positions x xs = find' x (zip xs [0..n])
    where n = length xs -1


scalarproduct xs ys = sum [x * y | (x,y) <- xs `zip` ys]

main :: IO ()
main = do
    print $ scalarproduct [1,2,3] [4,5,6]
