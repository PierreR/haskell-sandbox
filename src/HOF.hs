module HOF where

import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.List(foldl')

import Prelude hiding (foldl)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

replicate' 0 _ = []
replicate' n x = x : replicate (n-1) x

any' p xs = foldl (\ x acc -> (p x) || acc) False xs

all' p = foldr (&&) True . map p

and' [] = True
and' (x:xs)
  | x == False = False
  | otherwise = and xs

and'' [] = True
and'' (x:xs) = and xs && x

concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

takeWhile' p = foldl (\acc x -> if p x then x : acc else acc) []

dropWhile' _ [] = []
dropWhile' p (x:xs)
  | p x = dropWhile p xs
  | otherwise = x:xs

dropWhile'' p = Prelude.foldr (\ x acc -> if p x then acc else x : acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = Prelude.foldr (\ x xs -> if p x then xs ++ [x] else xs) []

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : (map f xs)

map'' :: (a -> b) -> [a] -> [b]
map'' f = Prelude.foldr (\x ys -> f x : ys) []

map''' :: (a -> b) -> [a] -> [b]
map''' f = foldl (\ys x -> f x : ys) []

dec2int = foldl (\x y -> 10 * x + y) 0

sum' :: [Int] -> Int
sum' = foldl' (+) 0

testme p f xs = [f x | x <- xs, p (f x)]

unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

type Bit = Int
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop8' = unfold null (take 8) (drop 8)

mapx' f = unfold null (f . head) tail

iterate' f = unfold (const False) id f

compose :: [a -> a] -> (a -> a)
compose = Prelude.foldr (.) id


curry' f = \ x y -> f (x, y)
uncurry' f = \ (x,y) -> f x y


