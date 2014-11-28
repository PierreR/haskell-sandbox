module IP where

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

-- putStrLn' :: String -> IO ()
-- putStrLn' [] = putChar '\n'
-- putStrLn' xs = putStr' xs >> putChar '\n'

interact' f = do input <- getLine
                 putStrLn (f input)

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM' f as = sequence (map f as)
mapM' _ [] = return []
mapM' f (a : as) =
  f a >>=
    \ b -> do bs <- mapM' f as
              return (b : bs)

mapM'' :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM' f as = sequence (map f as)
mapM'' _ [] = return []
mapM'' f (a : as) =
  f a >>=
    \ b -> do bs <- mapM' f as
              return (bs ++ [b])

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' p (x : xs) = do flag <- p x
                         ys <- filterM' p xs
                         if flag then return (x:ys) else return ys

foldLeftM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldLeftM _ acc [] = return acc
foldLeftM f acc (x:xs) = f acc x >>= (\ b' -> foldLeftM f b' xs)

foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM _ z [] = return z
foldRightM f z (x:xs) = (foldRightM f z xs) >>= f x

foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight _ z [] = z
foldRight f z (x:xs) = f x (foldRight f z xs)
-- foldLeft :: (a -> b -> a) -> a -> [b] -> a
-- foldLeft _ acc [] = acc
-- foldLeft f acc (x:xs) = foldLeft f (f acc x) xs

liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' f m = do x <- m
                return $ f x
