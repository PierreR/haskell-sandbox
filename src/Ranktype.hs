{-# LANGUAGE RankNTypes #-}
module Ranktype where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.State
import           System.Random

data Player = Player
  { _name :: String
  , _pos  :: (Double, Double)
  } deriving (Eq, Ord, Show)

type GenActionR m = forall a. (Random a) => (a, a) -> m a

genRandomR :: (RandomGen g) => GenActionR (State g)
genRandomR range = state (randomR range)

randomPlayer :: (MonadIO m) => GenActionR m -> m Player
randomPlayer genR = do
    liftIO (putStrLn "Generating random player...")

    len  <- genR (8, 12)
    name <- replicateM len (genR ('a', 'z'))
    x    <- genR (-100, 100)
    y    <- genR (-100, 100)

    liftIO (putStrLn "Done.")
    return (Player name (x, y))

main :: IO ()
main = randomPlayer randomRIO >>= print
