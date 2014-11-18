module HOF where

import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.List(foldl')

foldLeft :: (b -> a -> b) -> b -> [a] -> b
foldLeft _ acc [] = acc
foldLeft f acc (x:xs) = foldLeft f (f acc x) xs

any' p xs = Prelude.foldl (\ x acc -> (p x) || acc) False xs

takeWhile' p = Prelude.foldl (\acc x -> if p x then x : acc else acc) []

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
map''' f = Prelude.foldl (\ys x -> f x : ys) []

dec2int = Prelude.foldl (\x y -> 10 * x + y) 0

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

all' p = foldr (&&) True . map p

curry' f = \ x y -> f (x, y)
uncurry' f = \ (x,y) -> f x y


