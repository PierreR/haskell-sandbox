module Monoid where

import Data.Monoid
import Data.Foldable

import Prelude hiding (foldr)

findFirst :: Foldable t => (a -> Bool) -> t a -> Maybe a
findFirst p = getFirst . foldMap (\x -> if p x
                                then First (Just x)
                                else First Nothing)

sum'' :: (Foldable t, Num a) => t a -> a
sum'' = getSum . foldMap' Sum
  where
    foldMap' :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
    foldMap' f = foldr (mappend . f) mempty
