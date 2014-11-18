module Foldl where

import qualified Control.Foldl as L
import Data.Foldable

import Prelude hiding (foldr)


-- the fold function
fold :: Foldable f => L.Fold a b -> f a -> b
fold (L.Fold step begin done) as = foldr cons done as begin
  where
    cons a k x = k $! step x a