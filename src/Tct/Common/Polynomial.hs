{- | This module provides a tree-based implementation for multivariate polynomials.

All operations on 'Monomial' fulfill following invariants:

    * each variable (indeterminate) occurs at most once
    * exponents are >= 1

All operations on 'Polynomial' fulfill following invariants:

  * monomials are valid
  * monomials are unique
  * coefficients are non-zero wrt. 'zero'

Basic operations are defined via 'SemiRing' ('Ring') class.

For constructing polynomials a view 'PolynomialView' is provided.
A view is not as restrictive but does not support any arithmetical operations.
Use 'fromView' or 'fromViewWith' to construct a 'Polynomial'.

-}
module Tct.Common.Polynomial
  (
  -- * Polynomial
  Monomial
  , Polynomial
  , degree
  -- * View
  , PowerView (..)
  , MonomialView (..)
  , PolynomialView (..)
  , fromViewWith
  , fromView
  , (^^^), mono, poly
  , constant
  , variable
  , linear
  , quadratic
  , mixed
  -- * Pretty Printing
  , ppMonomial
  , ppPolynomial
  )
  where


import qualified Data.Map.Strict   as M

import qualified Tct.Common.Pretty as PP
import           Tct.Common.Ring


-- | The abstract monomial type.
newtype Monomial v = Mono (M.Map v Int)
  deriving (Eq, Ord)

mmult :: Ord v => Monomial v -> Monomial v -> Monomial v
mmult (Mono ps1) (Mono ps2) = Mono $ M.unionWith (+) ps1 ps2

mone :: Monomial v
mone = Mono M.empty

mdegree :: Monomial v -> Int
mdegree (Mono ps)
  | M.null ps = -1
  | otherwise = maximum $ M.elems ps

instance Ord v => Multiplicative (Monomial v) where
  one  = mone
  mult = mmult


-- | The abstract polynom type.
newtype Polynomial c v = Poly (M.Map (Monomial v) c)

pnormalise :: (Additive c, Eq c) => Polynomial c v -> Polynomial c v
pnormalise (Poly ts) = Poly $ M.filter (zero /=) ts

asConstant :: (SemiRing c, Eq v) => Polynomial c v -> Maybe c
asConstant (Poly ts) = case M.toList ts of
  []                  -> Just zero
  [(m,c)] | m == mone -> Just c
  _                   -> Nothing

pzero :: Polynomial c v
pzero = Poly M.empty

padd :: (Additive c, Eq c, Ord v) => Polynomial c v -> Polynomial c v -> Polynomial c v
padd (Poly ts1) (Poly ts2) = pnormalise . Poly $ M.unionWith add ts1 ts2

instance (Additive c, Eq c, Ord v) => Additive (Polynomial c v) where
  zero = pzero
  add  = padd

pone :: Multiplicative c => Polynomial c v
pone = Poly $ M.singleton mone one

pscale :: SemiRing c => c -> Polynomial c v -> Polynomial c v
pscale c (Poly ts) = Poly $ M.map (`mult` c) ts

pmult :: (SemiRing c, Eq c, Ord v) => Polynomial c v -> Polynomial c v -> Polynomial c v
pmult p1@(Poly ts1) p2@(Poly ts2) = pnormalise $ case (asConstant p1, asConstant p2) of
  (Just c, _) -> pscale c p2
  (_, Just c) -> pscale c p1
  _           ->
    Poly $ M.fromListWith add
      [ (m1 `mmult` m2, c1 `mult` c2) | (m1,c1) <- M.toList ts1, (m2,c2) <- M.toList ts2 ]

instance (SemiRing c, Eq c, Ord v) => Multiplicative (Polynomial c v) where
  one  = pone
  mult = pmult

pnegate :: AdditiveGroup c => Polynomial c v -> Polynomial c v
pnegate (Poly ms) = Poly $ M.map neg ms

instance (AdditiveGroup c, Eq c, Ord v) => AdditiveGroup (Polynomial c v) where
  neg = pnegate


-- | Returns the degree of the polynomial.
--
-- prop> degree zero = -1
degree :: Ord v => Polynomial c v -> Int
degree (Poly ms) = maximum (map mdegree $ M.keys ms)


--- * View -----------------------------------------------------------------------------------------------------------

-- | Power type with variable @v@.
data PowerView v           = PowV v Int
-- | Monomial type with coefficient @c@ and variable @v@.
data MonomialView c v      = MonoV c [PowerView v]
-- | Polynomial type with coefficient @c@ and variable @v@.
newtype PolynomialView c v = PolyV [MonomialView c v]

-- | prop> v^^^1 = PowV v i
(^^^) :: v -> Int -> PowerView v
v^^^i = PowV v i

-- | prop> mono = MonoV
mono :: Ord v => c -> [PowerView v] -> MonomialView c v
mono = MonoV

-- | prop> poly = PolyV
poly :: [MonomialView c v] -> PolynomialView c v
poly = PolyV

-- | @'fromViewWith' f g p@ first maps @f@ and @g@ over @p@ and then constructs a normalised monomial st. all invariants
-- hold. Non-positive exponents are ignored. Multiple occurences of a variable in a monomial or multiple occurences of the
-- same monomial are suitably combined.
fromViewWith :: (Additive c', Eq c', Ord v') => (c -> c') -> (v -> v') -> PolynomialView c v -> Polynomial c' v'
fromViewWith f g (PolyV ts) = pfromView f g ts

-- | Like 'fromViewWith' with the identity function applied.
--
-- prop> fromView = fromViewWith id id
fromView :: (Additive c, Eq c, Ord v) => PolynomialView c v -> Polynomial c v
fromView = fromViewWith id id

-- constructs a normalised monomial
mfromView :: Ord v' => (v -> v') -> [PowerView v] -> Monomial v'
mfromView g = Mono . foldr k M.empty
  where k (PowV v i) m = if i>0 then M.insertWith (+) (g v) i m else m

-- constructs a normalised polynomial
pfromView :: (Additive c', Eq c', Ord v') => (c -> c') -> (v -> v') -> [MonomialView c v] -> Polynomial c' v'
pfromView f g = Poly . foldr k M.empty where
  k (MonoV c ps) m = let c' = f c in
    if c' /= zero then M.insertWith add (mfromView g ps) c' m else m

-- | Lifts a constant to a polynom.
constant :: c -> PolynomialView c v
constant c = PolyV [MonoV c []]

-- | Lifts a variable to a polynom (with exponent 1).
variable :: Multiplicative c => v -> PolynomialView c v
variable v = PolyV [MonoV one [PowV v 1]]

-- | @'linear' f [x,...,z] = cx*x + ... + cz*z + c@
-- constructs a linear polynomial; the coefficients are determinded by applying @f@ to each monomial.
linear :: Ord v => ([PowerView v] -> c) -> [v] -> PolynomialView c v
linear f = poly . (mkMono [] :) . map (\v -> mkMono [v^^^1])
  where mkMono ps = mono (f ps) ps

-- | @'quadratic' f [x,...,z] = cx2*x^2 + cx*x + ... + cz2*z^2 + cz*z + c@
-- constructs a quadratic polynomial; the coefficients are determined by applying @f@ to each monomial.
quadratic :: Ord v => ([PowerView v] -> c) -> [v] -> PolynomialView c v
quadratic f = poly . (mkMono [] :) . map (\v -> mkMono [v^^^2,v^^^1])
  where mkMono ps = mono (f ps) ps

-- | Creates a mixed polynom up to a specified degree; the coefficients are determined by applying @f@ to each monomial.
--
-- > mixed (const 1) "xz" 2 = x^2 + x*z + x + z^2 + z + 1
mixed :: Ord v => ([PowerView v] -> c) -> [v] -> Int -> PolynomialView c v
mixed f vs d =  poly $ map mkMono pows
  where
    mkMono ps = mono (f ps) ps
    pows =
      map (filter (\(PowV _ i) -> i>0) . zipWith PowV vs) -- [] isElem of pows
      . filter (\ps -> sum ps <= d)
      . sequence $ replicate (length vs) [0..d]


--- * Pretty ---------------------------------------------------------------------------------------------------------
instance PP.Pretty v => PP.Pretty (Monomial v) where
  pretty = ppMonomial PP.pretty

instance (PP.Pretty c, PP.Pretty v) => PP.Pretty (Polynomial c v) where
  pretty = ppPolynomial PP.pretty PP.pretty

-- | Pretty printing function for 'Monomial'.
-- Should be used for basic types that do not provide the 'Pretty' instance.
ppMonomial :: (v -> PP.Doc) -> Monomial v -> PP.Doc
ppMonomial pp (Mono ps)
  | M.null ps = PP.empty
  | otherwise = PP.hcat . PP.punctuate (PP.char '*') . map k $ M.toAscList ps
    where k (v,i) = pp v PP.<> PP.char '^' PP.<> PP.int i

-- | Pretty printing function for 'Polynomial'.
-- Should be used for basic types that do not provide the 'Pretty' instance.
ppPolynomial  :: (c -> PP.Doc) -> (v -> PP.Doc) -> Polynomial c v -> PP.Doc
ppPolynomial ppr ppv (Poly ts) = PP.hcat . PP.punctuate (PP.text " + ") $ map k $ M.toAscList ts
  where k (m@(Mono ps),c) = ppr c PP.<> if M.null ps then PP.empty else PP.char '*' PP.<> ppMonomial ppv m

