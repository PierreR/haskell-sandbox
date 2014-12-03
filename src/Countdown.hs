module Countdown where

import           Control.Monad (guard)
import           Data.List     (permutations)
import           Data.Monoid   ((<>))

data Expr = Val Int
          | App Op Expr Expr

data Op = Add
        | Sub
        | Mul
        | Div

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

-- eval :: Expr -> Maybe Int
-- eval (Val n) = if n > 0 then Just n else Nothing
-- eval (App o l r) = do
--   x <- eval l
--   y <- eval r
--   guard (valid o x y)
--   return $ apply o x y

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l
                                , y <- eval r
                                , valid o x y]

choices :: Ord a =>[a] -> [[a]]
choices xs =  [[]] <> [[x]| x <- xs] <> (permutations xs)
