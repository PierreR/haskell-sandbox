{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}
-- | Main entry point to the application.
module Main where

import           Control.Monad              (guard)
import           Data.Char                  (digitToInt, ord, chr, isLower, isUpper)
import           Data.List                  (sort)

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ smaller
    where smaller = filter (< x) xs
          larger  = filter (>=x) xs

--digitize :: Integer -> [Integer]
digitize i = map digitToInt (sort $ show i)

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop (n+1)  xs)
    where n = (length xs) `div` 2

halve' xs = splitAt (length xs `div` 2)

safeTail :: [a] -> [a]
safeTail xs = if null xs then [] else tail xs

trues a b = if a then if b then True else False else False

factors :: Int -> [Int]
factors n = [x | x <- [1 ..n], n `mod` x == 0]

factors' n = do
    x <- [1..n]
    guard $ n `mod` x == 0
    return x

perfects n = [x | x <- [1..n], isPerfect x]
isPerfect num = sum (factors num) == num

-- this is equal to [(x,y) | x <- [1,2,3], y <- [4,5,6]]
comprehension = concat [ [(x,y) | y <- [4,5,6]] | x <- [1,2,3]]

isPrime :: Int -> Bool
isPrime n = factors n == [1,n]

primes = [ x | x <- [1..], isPrime x]

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

riffle xs ys = concat [[x,y] | (x,y) <- xs `zip` ys]

find' k t = [v | (k', v) <- t, k == k']

positions x xs = find' x (zip xs [0..n])
    where n = length xs -1


scalarproduct xs ys = sum [x * y | (x,y) <- xs `zip` ys]

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let n = chr (ord 'a' + n)

shift n c
    | isLower c = int2let ((let2int c + n) `mod` 26)
    | isUpper c = chr (ord 'A' + ((ord c - ord 'A' + n) `mod` 26))
    | otherwise = c


encode n xs = [shift n x | x <- xs]

main :: IO ()
main = do
    print $ encode 13 "Think like a Fundamentalist Code like a Hacker"
