module Tct.Certificate (
  -- * Complexity Functions
  Complexity (..)
  , add
  , mult
  , compose
  , iter
  -- * Certificates
  , Certificate (..)
  , unbounded
) where

import Tct.SemiRing

data Complexity 
    = Poly (Maybe Int) -- ^ Polynomial. If argument is @Just k@, then 
                       -- @k@ gives the degree of the polynomial
    | Exp (Maybe Int) -- ^ Exponential. If argument is @Nothing@, then 
                      -- represented bounding function is elementary. If argument 
                      -- is @Just k@, then bounding function is k-exponential. 
                      -- Hence @Exp (Just 1)@ represents an exponential bounding 
                      -- function. 
    | Supexp -- ^ Super exponential.
    | Primrec -- ^ Primitive recursive.
    | Multrec -- ^ Multiple recursive.
    | Rec -- ^ Recursive.
    | Unknown -- ^ Unknown.
      deriving (Eq, Show)

rank :: Complexity -> (Int, Int)
rank (Poly (Just r)) = (42,r)
rank (Poly _)        = (43,0)
rank (Exp (Just r))  = (44,r)
rank (Exp _)         = (45,0)
rank Supexp          = (46,0)
rank Primrec         = (47,0)
rank Multrec         = (48,0)
rank Rec             = (49,0)
rank Unknown         = (142,0)

instance Ord Complexity where
  c1 <= c2 = a1 < a2 || (a1 == a2 && b1 <= b2)
    where (a1,b1) = rank c1
          (a2,b2) = rank c2

instance Additive Complexity where
    add = max
    zero = Poly (Just 0)

instance Multiplicative Complexity where
    (Poly (Just n)) `mult` (Poly (Just m)) = Poly $ Just $ n + m
    (Poly Nothing)  `mult` (Poly _)        = Poly Nothing
    (Poly _)        `mult` (Poly Nothing)  = Poly Nothing
    (Exp (Just n))  `mult` (Exp (Just m))  = Exp $ Just $ max n m
    (Exp Nothing)   `mult` (Exp _)         = Exp Nothing
    a               `mult` b               = max a b
    one = Poly (Just 0)

compose :: Complexity -> Complexity -> Complexity
(Poly (Just n)) `compose` a
    | n == 0 = Poly (Just 0)
    | n == 1 = a
a `compose` (Poly (Just m)) 
    | m == 0 = Poly (Just 0)
    | m == 1 = a
(Poly (Just n)) `compose` (Poly (Just m)) = Poly $ Just $ n * m
(Poly Nothing) `compose` (Poly _) = Poly Nothing
(Poly _) `compose` (Poly Nothing) = Poly Nothing
(Exp (Just n)) `compose` (Poly _) = Exp $ Just $ n + 1
(Poly _) `compose` (Exp (Just m)) = Exp $ Just m
(Exp (Just n)) `compose` (Exp (Just m)) = Exp $ Just $ n + m
(Exp Nothing) `compose` (Exp _) = Exp Nothing
(Exp _) `compose` (Exp Nothing) = Exp Nothing
a `compose` b = maximum [Primrec, a, b]

iter :: Complexity -> Complexity -> Complexity
(Poly (Just n)) `iter` _ 
    | n == 0 = Poly $ Just 0
(Poly (Just n)) `iter` (Poly m) 
    | n == 1 = 
        case m of
          Just 0 -> Poly $ Just 1
          Just 1 -> Exp $ Just 1
          _ -> Exp $ Just 2
(Poly n) `iter` (Exp _) 
    | n == Just 0 = Exp Nothing
    | n == Just 1 = Supexp
    | otherwise = Primrec
(Poly _) `iter` b = max Primrec b
(Exp _) `iter` (Poly m) 
    | m == Just 0 = Exp Nothing
    | m == Just 1 = Supexp
    | otherwise = Primrec
a `iter` b = maximum [Primrec, a, b]

data Certificate =
    Certificate { spaceUB :: Complexity
                , spaceLB :: Complexity
                , timeUB  :: Complexity
                , timeLB  :: Complexity }

unbounded :: Certificate
unbounded = 
    Certificate { spaceUB = Unknown
                , spaceLB = Unknown
                , timeUB = Unknown
                , timeLB = Unknown }


                 
