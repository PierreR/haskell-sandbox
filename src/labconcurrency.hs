module Main where

import           Control.Monad

newtype Concurrent a = Concurrent {
  unwrap :: (a -> Action) -> Action
}

data Action
    = Atom (IO Action)
    | Fork Action Action
    | Stop

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- ===================================
-- Ex. 0
-- ===================================
-- | we use Stop to create the continuation that is provided to f
action :: Concurrent a -> Action
action (Concurrent f) = f (\_ -> Stop)


-- ===================================
-- Ex. 1
-- ===================================
-- | we disregard any continuation and return Stop as the Action
stop :: Concurrent a
stop = Concurrent (\k -> Stop)


-- ===================================
-- Ex. 2
-- ===================================
atom':: IO a -> ((a -> Action) -> Action)
atom' ma = \k -> Atom (ma >>= (\a -> return $ k a))

atom :: IO a -> Concurrent a
atom ma = Concurrent (\k -> Atom (ma >>= \a -> return $ k a))


-- ===================================
-- Ex. 3
-- ===================================

fork :: Concurrent a -> Concurrent ()
fork (Concurrent f) = Concurrent $ \c -> c ()

par :: Concurrent a -> Concurrent a -> Concurrent a
par (Concurrent f) (Concurrent g) = Concurrent $ \k -> Fork (f k) (g k)


-- ===================================
-- Ex. 4
-- ===================================

instance Monad Concurrent where
    (Concurrent f) >>= g = error "You have to implement >>="
    return x = Concurrent (\c -> c x)


-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
roundRobin = error "You have to implement roundRobin"

-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331)
         loop $ genRandom 42
         atom (putStrLn "")


-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs

main = undefined
