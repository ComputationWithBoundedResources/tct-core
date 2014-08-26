module Tct.SemiRing
  (
    Additive (..)
  , Multiplicative (..)
  , SemiRing
  ) where


-- | The commutative monoid underlying a 'SemiRing'.
class Additive a where
    add :: a -> a -> a
    zero :: a

-- | The monoid underlying a 'SemiRing'.
class Multiplicative a where
  mult :: a -> a -> a
  one :: a

-- | The 'Additive' and 'Multiplicative' instances
-- should satisfy the following laws:
--
-- * (a,'add') is a commutative monoid with identity 'zero':
--
--      prop> a `add` (b `add` c) = (a `add` b) `add` c
--      prop> a `add` b = b `add` a
--      prop> zero `add` a = a `add` zero = a
--
-- * (a, 'mult') is a monoid with identity 'one':
--
--      prop> a `mult` (b `mult` c) = (a `mult` b) `mult` c
--      prop> one `mult` a = a `mult` one = a
--
-- * 'mult' left and right distributes over 'add':
--
--      prop> a `mult` (b `add` c) = (a `mult` b) `add` (a `mult` c)
--      prop> (a `add` b) `mult` c = (a `mult` c) `add` (b `mult` c)
--
-- * 'mult' by 'zero' annihilates a
--
--      prop> zero `mult` a = a `mult` zero = zero
type SemiRing a = (Additive a, Multiplicative a)

