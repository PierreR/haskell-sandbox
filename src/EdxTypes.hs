module EdxTypes where

length' []     = 0
length' (_:xs) = 1 + length' xs

data Tree = Leaf Integer
          | Node Tree Tree deriving Show

balanced :: Tree -> Bool
balanced (Leaf _) = True
balanced (Node l r) =
  abs (leaves l - leaves r) <= 1
  where
    leaves (Leaf _) = 1
    leaves (Node l r) = leaves l + leaves r

balance :: [Integer] -> Tree
balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs)
    where
      (ys,zs) = halve xs
halve :: [Integer] -> ([Integer], [Integer])
halve xs = splitAt (length xs `div` 2) xs

concatenate [] ys = ys
concatenate (x:xs) ys = x: concatenate xs ys

t' = [Just undefined, Just "Hello"]
main = print $ concatenate t' [Just "Me"]
