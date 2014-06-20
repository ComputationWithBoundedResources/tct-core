module Tct.Core.Forks where 

import Tct.Core.Types 

import Control.Applicative 
import Data.Foldable as F
import Data.Traversable as T


newtype Pair a = Pair (a,a)

instance Functor Pair where 
    fmap f (Pair (a,b)) = Pair (f a, f b)
instance Foldable Pair where
    foldr f e (Pair (a,b)) = a `f` (b `f` e)
instance Traversable Pair where
    traverse f (Pair (a,b)) = Pair <$> liftA2 (,) (f a) (f b)

