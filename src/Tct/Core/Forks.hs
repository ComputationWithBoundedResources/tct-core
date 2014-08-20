module Tct.Core.Forks
  (
  -- * Forks
    Judgement (..)
  , judgement
  , Id (..)
  , Pair (..)
  )
where


import Control.Applicative (liftA2, (<$>))
import Data.Foldable       as F (Foldable (..))
import Data.Traversable    as T (Traversable (..))


-- | 'Judgement' representst a node with no successor.
data Judgement a = Judgement deriving (Foldable,Functor,Traversable)

-- | Convenience function that ignores the Judgement.
-- prop> judgment a Judgment = a
judgement :: a -> Judgement b -> a
judgement a _ = a

-- | 'Id' represents a node with one successor.
newtype Id a = Id a
  deriving (Foldable, Functor, Traversable)

-- | 'Pair' represents a node with two successors.
newtype Pair a = Pair (a,a)

instance Functor Pair     where f `fmap` (Pair (a,b)) = Pair (f a, f b)
instance Foldable Pair    where foldr f e (Pair (a,b)) = a `f` (b `f` e)
instance Traversable Pair where f `traverse` (Pair (a,b)) = Pair <$> liftA2 (,) (f a) (f b)

