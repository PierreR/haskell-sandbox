module Main where

import           Control.Monad (guard)
import qualified Data.List     as List
import           Data.Monoid   ((<>))

data Expr = Val Int
          | App Op Expr Expr

instance Show Expr where
   show (Val n)               =  show n
   show (App o l r)           =  bracket l ++ show o ++ bracket r
                                 where
                                    bracket (Val n) = show n
                                    bracket e       = "(" ++ show e ++ ")"
data Op = Add
        | Sub
        | Mul
        | Div

instance Show Op where
   show Add                   =  "+"
   show Sub                   =  "-"
   show Mul                   =  "*"
   show Div                   =  "/"

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x <= y && x /= 1 && y /= 1
valid Div x y = x `mod` y == 0 && y /= 1

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
-- TODO: Clean it up
choices :: Ord a => [a] -> [[a]]
choices xs = go xs []
   where
     go [] _ = [[]]
     go [x] _ = [] : [[x]]
     go l@(y:ys) done = extraperm l done <> List.permutations l  <> go ys (y:done)

     extraperm :: [a] -> [a] -> [[a]]
     extraperm (z:_) [] = [[z]]
     extraperm (z:zs) (w:ws) = List.permutations (w:zs) <> extraperm (z:zs) ws


split :: [a] -> [([a],[a])]
split [] = error "splitting empty list make no sense"
split (x:xs) = go [x] xs
  where
    go z [y] = [(z,[y])]
    go z l@(y:ys) = (z, l) : go (z <> [y]) ys

exprs :: [Int] -> [Expr]
exprs []  =  []
exprs [n] =  [Val n]
exprs ns  =  [e | (ls,rs) <- split ns
                 , l      <- exprs ls
                 , r      <- exprs rs
                 , e      <- combine l r]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results []  =  []
results [n] =  [(Val n, n) | n > 0]
results ns  =  [ res | (ls,rs) <- split ns
               , lx            <- results ls
               , ry            <- results rs
               , res           <- combine' lx ry]

combine :: Expr -> Expr -> [Expr]
combine l r = [ App o l r | o <- [Add, Sub, Mul, Div]]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [ (App o l r, apply o x y ) | o <- [Add, Sub, Mul, Div]
                                                     , valid o x y ]


values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l <> values r

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns)
               && eval e == [n]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns'      <- choices ns
                     , (e, m)   <- results ns'
                     , m == n]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns
                    , e   <- exprs ns'
                    , eval e == return n]

-- testing
solExprs_1 = App Mul (App Sub (Val 25) (Val 10)) (App Add (Val 50) (Val 1))
solExprs_2 = App Mul (App Sub (App Add (Val 7) (Val 3)) (Val 1)) (App Add (Val 25) (App Add (Val 50) (Val 10)))

inputNumbers :: [Int]
inputNumbers = [1,3,7,10,25,50]

target :: Int
target = 765

main =
  mapM print $ solutions' inputNumbers target
