module Tct.Core.Forks
  ( 
  -- * Forks
    Judgement (..)
  , judgement
  , Id (..)
  , Pair (..)
  )
where

import Control.Applicative ((<$>), liftA2)
import Data.Foldable as F
import Data.Traversable as T

-- | 'Judgement' describes a node with no successor.
data Judgement a = Judgement deriving (Foldable,Functor,Traversable)

-- | Convenience function that ignores the Judgement.
-- prop> judgment a Judgment = a
judgement :: a -> Judgement b -> a
judgement a _ = a

-- | 'Id' describes a node with one successor.
newtype Id a = Id a 
  deriving (Foldable, Functor, Traversable)

-- | 'Pair' describes a node with two successors.
newtype Pair a = Pair (a,a)

instance Functor Pair where
    fmap f (Pair (a,b)) = Pair (f a, f b)
instance Foldable Pair where
    foldr f e (Pair (a,b)) = a `f` (b `f` e)
instance Traversable Pair where
    traverse f (Pair (a,b)) = Pair <$> liftA2 (,) (f a) (f b)

