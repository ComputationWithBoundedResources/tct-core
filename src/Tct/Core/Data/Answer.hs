-- | This module provides standard output formats for certificates.
module Tct.Core.Data.Answer
  (
  Timebounds (..)
  , timebounds
  -- * tttac / termcomp (prior 2015) format
  , TTTAC (..)
  , tttac
  -- * termcomp 2015 format
  , Termcomp (..)
  , termcomp
  ) where


import           Data.Monoid               ((<>))
import qualified Tct.Core.Common.Pretty    as PP
import qualified Tct.Core.Common.Xml       as Xml

import qualified Tct.Core.Data.Certificate as T


-- | Timebounds
data Timebounds = Timebounds
  T.Complexity  -- ^ lower bound
  T.Complexity  -- ^ upper bound

-- | Extracts lowwer and upper bounds of a certificate.
timebounds :: T.Certificate -> Timebounds
timebounds c = Timebounds (T.timeLB c) (T.timeUB c)

instance PP.Pretty Timebounds where
  pretty (Timebounds lb  ub) = PP.text "Timebounds" <> PP.tupled [PP.pretty lb, PP.pretty ub]

instance Xml.Xml Timebounds where
  toXml (Timebounds lb ub) = Xml.elt "timebounds"
    [ Xml.elt "lowerbound" [Xml.toXml lb]
    , Xml.elt "upperbound" [Xml.toXml ub] ]


--- * termcomp -------------------------------------------------------------------------------------------------------

-- | Newtype wrapper for 'Timebounds'.
-- The pretty printing instance corresponds to the old (prior 2015) /termcomp/ format and is compatible with the
-- /tttac/ testing tool.
newtype TTTAC = TTTAC Timebounds

-- | Returns the certificate in a /tttac/ compatible format.
tttac :: T.Certificate -> TTTAC
tttac = TTTAC . timebounds

instance PP.Pretty TTTAC where
  pretty (TTTAC (Timebounds lb  ub))
    | lb /= T.Unknown || ub /= T.Unknown = PP.text "YES" <> PP.tupled [PP.pretty lb, PP.pretty ub]
  pretty _ = PP.text "MAYBE"

instance Xml.Xml TTTAC where
  toXml (TTTAC (Timebounds lb ub))
    | lb /= T.Unknown || ub /= T.Unknown = Xml.elt "certified"
      [ Xml.elt "lowerbound" [Xml.toXml lb]
      , Xml.elt "upperbound" [Xml.toXml ub] ]
  toXml _  = Xml.elt "maybe" []


--- * termcomp -------------------------------------------------------------------------------------------------------

-- | Newtype wrapper for 'Timebounds'.
-- The pretty printing instance corresponds to the /termcomp 2015/ format.
-- See <http://cbr.uibk.ac.at/competition/rules.php> (September 2015) for more information.
newtype Termcomp = Termcomp Timebounds

-- | Returns the certificate in the /termcomp 2015/ format.
termcomp :: T.Certificate -> Termcomp
termcomp = Termcomp . timebounds

toTermcomp :: t -> ((t1, t1) -> t) -> (Int -> t1) -> t1 -> (Int -> t1) -> t1 -> t1 -> Termcomp -> t
toTermcomp maybeA worst omegaA npolyA oA polyA unknownA (Termcomp (Timebounds  lb  ub)) = case (normlb lb, normub ub) of
  (T.Unknown, T.Unknown) -> maybeA
  (nlb, nub)             -> worst (toclb nlb, tocub nub)
  where
    toclb T.Unknown         = unknownA
    toclb (T.Poly (Just i)) = omegaA i
    toclb _                 = npolyA

    tocub T.Unknown         = unknownA
    tocub (T.Poly (Just i)) = oA i
    tocub _                 = polyA

    normlb p@(T.Poly (Just i)) | i > 0 = p
    normlb e@(T.Exp _)                 = e
    normlb _                           = T.Unknown

    normub p@(T.Poly (Just i)) | i >= 0 = p
    normub p@(T.Poly Nothing)           = p
    normub _                            = T.Unknown

instance PP.Pretty Termcomp where
  pretty =
    toTermcomp
      (PP.text "MAYBE")
      (\(lb,ub) -> PP.text "WORST_CASE" <> PP.tupled [lb,ub])
      (\i -> PP.text "Omega" <> PP.parens (PP.text "n^" <> PP.int i))
      (PP.text "NON_POLY")
      (\i -> PP.char 'O' <> PP.parens (if i == 0 then PP.int 1 else PP.text "n^" <> PP.int i))
      (PP.text "POLY")
      (PP.char '?')

instance Xml.Xml Termcomp where
  toXml =
    toTermcomp
      (Xml.elt "maybe" [])
      (\(lb,ub) -> Xml.elt "worst_case" [Xml.elt "lowerbound" [lb], Xml.elt "upperbound" [ub]])
      (\i -> Xml.elt "polynomial" [Xml.int i])
      (Xml.text "non_poly")
      (\i -> Xml.elt "polynomial" [Xml.int i])
      (Xml.text "poly")
      (Xml.text "unknown")

