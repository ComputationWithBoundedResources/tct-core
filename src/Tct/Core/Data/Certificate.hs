-- | This module provides the 'Complexity' and 'Certificate' type.
module Tct.Core.Data.Certificate
  (
  -- * Complexity Functions
  Complexity (..)
  , constant
  , linear

  -- * Semiring/Composition
  , compose
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


import Data.Monoid
import qualified Tct.Core.Common.Pretty as PP
import qualified Tct.Core.Common.Xml    as Xml
import Tct.Core.Common.SemiRing


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
  (Poly (Just n)) `mul` (Poly (Just m)) = Poly $ Just $ n + m
  (Poly Nothing)  `mul` (Poly _)        = Poly Nothing
  (Poly _)        `mul` (Poly Nothing)  = Poly Nothing
  (Exp (Just n))  `mul` (Exp (Just m))  = Exp $ Just $ max n m
  (Exp Nothing)   `mul` (Exp _)         = Exp Nothing
  a               `mul` b               = max a b
  one = Poly (Just 0)


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


{-
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

instance Additive Certificate where
  add c1 c2 = Certificate
    { spaceUB = spaceUB c1 `add` spaceUB c2
    , spaceLB = spaceLB c1 `add` spaceLB c2
    , timeUB = timeUB c1 `add` timeUB c2
    , timeLB = timeLB c1 `add` timeLB c2 }
  zero = Certificate zero zero zero zero

instance Multiplicative Certificate where
  mul c1 c2 = Certificate
    { spaceUB = spaceUB c1 `mul` spaceUB c2
    , spaceLB = spaceLB c1 `mul` spaceLB c2
    , timeUB = timeUB c1 `mul` timeUB c2
    , timeLB = timeLB c1 `mul` timeLB c2 }
  one = Certificate zero zero zero zero

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


-- Proof Data --------------------------------------------------------------------------------------------------------

instance PP.Pretty Complexity where
  pretty c = case c of 
    (Poly (Just 0)) -> asym $ PP.text "1"
    (Poly (Just k)) -> asym $ PP.text "n" <> PP.char '^' <> PP.int k
    (Poly Nothing)  -> PP.text "POLY"
    (Exp Nothing)   -> PP.text "ELEM"
    (Exp (Just 1))  -> PP.text "EXP"
    (Exp (Just k))  -> PP.text "EXP-" <> PP.int k
    Supexp          -> PP.text "SUPEXP"
    Primrec         -> PP.text "PRIMREC"
    Multrec         -> PP.text "MULTREC"
    Rec             -> PP.text "REC"
    Unknown         -> PP.char '?'
    where asym p = PP.char 'O' <> PP.parens p

instance PP.Pretty Certificate where
  pretty (Certificate su sl tu tl) =
    PP.text "TIME (" <> PP.pretty tu <> PP.char ',' <> PP.pretty tl <> PP.char ')' PP.<$$>
    PP.text "SPACE(" <> PP.pretty sl <> PP.char ',' <> PP.pretty su <> PP.char ')'

instance Xml.Xml Complexity where
  toXml c = case c of 
    (Poly Nothing)  -> Xml.elt "polynomial" []
    (Poly (Just k)) -> Xml.elt "polynomial" [Xml.int k]
    (Exp Nothing)   -> Xml.elt "exponential" []
    (Exp (Just k))  -> Xml.elt "exponential" [Xml.int k]
    Supexp          -> Xml.elt "superexponential" []
    Primrec         -> Xml.elt "primitiverecursive" []
    Multrec         -> Xml.elt "multiplerecursive" []
    Rec             -> Xml.elt "recursive" []
    Unknown         -> Xml.elt "unknown" []

