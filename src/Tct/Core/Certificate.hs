-- | This module provides the 'Complexity' and 'Certificate' type.
module Tct.Core.Certificate
  (
  -- * Complexity Functions
  Complexity (..)
  , constant
  , linear

  -- * Semiring/Composition
  , zero
  , add
  , one
  , mult
  -- , compose
  -- , iter

  -- * Certificates
  , Certificate (..)
  , unbounded

  -- * Setter/Getter
  , spaceUBCert
  , spaceLBCert
  , timeUBCert
  , timeLBCert
  , updateSpaceUBCert
  , updateSpaceLBCert
  , updateTimeUBCert
  , updateTimeLBCert
  ) where


import Tct.Common.Pretty
import Tct.Common.SemiRing


data Complexity
  = Poly (Maybe Int) -- ^ Polynomial. If argument is @Just k@, then
                     --   @k@ gives the degree of the polynomial
  | Exp (Maybe Int)  -- ^ Exponential. If argument is @Nothing@, then
                     --   represented bounding function is elementary. If argument
                     --   is @Just k@, then bounding function is k-exponential.
                     --   Hence @Exp (Just 1)@ represents an exponential bounding
                     --   function.
  | Supexp           -- ^ Super exponential.
  | Primrec          -- ^ Primitive recursive.
  | Multrec          -- ^ Multiple recursive.
  | Rec              -- ^ Recursive.
  | Unknown          -- ^ Unknown.
  deriving (Eq, Show)

-- | prop> constant = Poly (Just 0)
constant :: Complexity
constant = Poly (Just 0)

-- | prop> linear = Poly (Just 1)
linear :: Complexity
linear = Poly (Just 1)

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
  add  = max
  zero = Poly (Just 0)

instance Multiplicative Complexity where
  (Poly (Just n)) `mult` (Poly (Just m)) = Poly $ Just $ n + m
  (Poly Nothing)  `mult` (Poly _)        = Poly Nothing
  (Poly _)        `mult` (Poly Nothing)  = Poly Nothing
  (Exp (Just n))  `mult` (Exp (Just m))  = Exp $ Just $ max n m
  (Exp Nothing)   `mult` (Exp _)         = Exp Nothing
  a               `mult` b               = max a b
  one = Poly (Just 0)

{-
TODO: check
compose :: Complexity -> Complexity -> Complexity
(Poly (Just n)) `compose` a
  | n == 0 = Poly (Just 0)
  | n == 1 = a
a `compose` (Poly (Just m))
  | m == 0 = Poly (Just 0)
  | m == 1 = a
(Poly (Just n)) `compose` (Poly (Just m)) = Poly . Just $ n * m
(Poly Nothing)  `compose` (Poly _)        = Poly Nothing
(Poly _)        `compose` (Poly Nothing)  = Poly Nothing
(Exp (Just n))  `compose` (Poly _)        = Exp . Just $ n + 1
(Poly _)        `compose` (Exp (Just m))  = Exp $ Just m
(Exp (Just n))  `compose` (Exp (Just m))  = Exp . Just $ n + m
(Exp Nothing)   `compose` (Exp _)         = Exp Nothing
(Exp _)         `compose` (Exp Nothing)   = Exp Nothing
a               `compose` b               = maximum [Primrec, a, b]


iter :: Complexity -> Complexity -> Complexity
(Poly (Just n)) `iter` _
  | n == 0 = Poly $ Just 0
(Poly (Just n)) `iter` (Poly m)
  | n == 1 = case m of
    Just 0 -> Poly $ Just 1
    Just 1 -> Exp $ Just 1
    _      -> Exp $ Just 2
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
-}

-- | A fixed type for the complexity 'Certificate'.
data Certificate = Certificate
  { spaceUB :: Complexity
  , spaceLB :: Complexity
  , timeUB  :: Complexity
  , timeLB  :: Complexity
  } deriving Show

-- | Defines the identity 'Certificate'. Sets all components to 'Unknown'.
unbounded :: Certificate
unbounded = Certificate
  { spaceUB = Unknown
  , spaceLB = Unknown
  , timeUB  = Unknown
  , timeLB  = Unknown }

-- | Constructs a 'Certificate' from the given 'Complexity'.
-- Sets only the specified component; all others are set to 'Unknown'.
spaceUBCert, spaceLBCert, timeUBCert, timeLBCert :: Complexity -> Certificate
spaceUBCert c = unbounded { spaceUB = c }
spaceLBCert c = unbounded { spaceLB = c }
timeUBCert c  = unbounded { timeUB  = c }
timeLBCert c  = unbounded { timeLB  = c }

-- | Updates a component in the 'Certificate'.
updateSpaceUBCert, updateSpaceLBCert, updateTimeUBCert, updateTimeLBCert
  :: Certificate -> (Complexity -> Complexity) -> Certificate
updateSpaceUBCert cert f = cert { spaceUB = f $ spaceUB cert }
updateSpaceLBCert cert f = cert { spaceLB = f $ spaceLB cert }
updateTimeUBCert  cert f = cert { timeUB  = f $ timeUB  cert }
updateTimeLBCert  cert f = cert { timeLB  = f $ timeLB  cert }


-- Pretty Printing ---------------------------------------------------------------------------------------------------

instance Pretty Certificate where
  pretty (Certificate su sl tu tl) =
    text "TIME (" <> pretty tu <> char ',' <> pretty tl <> char ')' <$$>
    text "SPACE(" <> pretty sl <> char ',' <> pretty su <> char ')'

instance Pretty Complexity where
  pretty (Poly (Just 0)) = text "n"
  pretty (Poly (Just k)) = text "n" <> char '^' <> int k
  pretty (Poly Nothing)  = text "Poly"
  pretty (Exp Nothing)   = text "Elem"
  pretty (Exp (Just 1))  = text "Exp"
  pretty (Exp (Just k))  = text "Exp-" <> int k
  pretty Supexp          = text "Supexp"
  pretty Primrec         = text "Primrec"
  pretty Multrec         = text "Multrec"
  pretty Rec             = text "Rec"
  pretty Unknown         = char '?'

