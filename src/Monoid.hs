module Monoid where

import Data.Monoid
import Data.Foldable

findFirst :: Foldable t => (a -> Bool) -> t a -> Maybe a
findFirst p = getFirst . foldMap (\x -> if p x
                                then First (Just x)
                                else First Nothing)

