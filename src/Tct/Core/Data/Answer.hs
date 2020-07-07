-- | This module provides standard output formats for a 'Certificate'.
module Tct.Core.Data.Answer
  (
  -- * Generic timebound format.
    Timebounds(..)
  , timebounds
  , prettyDefaultFormatSep
  -- * tttac / termcomp (prior 2015) format
  , TTTAC(..)
  , tttac
  -- * termcomp 2015 format
  , Termcomp(..)
  , termcomp
  , DefaultFormat(..)
  , certificate2Default
  )
where


import           Data.Monoid                    ( (<>) )
import qualified Tct.Core.Common.Pretty        as PP
import qualified Tct.Core.Common.Xml           as Xml

import qualified Tct.Core.Data.Certificate     as T


-- | @Timebounds lower upper@
data Timebounds = Timebounds T.Complexity T.Complexity -- T.Complexity T.Complexity

-- | Extracts lower and upper bounds of a certificate.
timebounds :: T.Certificate -> Timebounds
timebounds c@T.Certificate{}    = Timebounds (T.timeLB c) (T.timeUB c)
timebounds T.CertificateYesNo{} = Timebounds T.Unknown T.Unknown

-- | Extracts lower and upper bounds of a certificate.
timeboundsBestCase :: T.Certificate -> Timebounds
timeboundsBestCase c@T.Certificate{} = Timebounds (T.timeBCLB c) (T.timeBCUB c)
timeboundsBestCase T.CertificateYesNo{} = Timebounds T.Unknown T.Unknown


instance PP.Pretty Timebounds where
  pretty (Timebounds lb ub) =
    PP.text "Timebounds" <> PP.tupled [PP.pretty lb, PP.pretty ub]

instance Xml.Xml Timebounds where
  toXml (Timebounds lb ub) = Xml.elt
    "timebounds"
    [Xml.elt "lowerbound" [Xml.toXml lb], Xml.elt "upperbound" [Xml.toXml ub]]


--- * old termcomp format -------------------------------------------------------------------------------------------------------

-- | Newtype wrapper for 'Timebounds'.
-- The pretty printing instance corresponds to the old (prior 2015) /termcomp/ format and is compatible with the
-- /tttac/ testing tool.
data TTTAC = TTTAC Timebounds
           | TTTACYN Bool

-- | Returns the certificate in a /tttac/ compatible format.
--
-- > pretty $ tttac unbounded = "MAYBE"
-- > pretty $ tttac (timeLBCert linear) = "YES(O(n^1),?)"
-- > pretty $ tttac (timeUBCert linear) = "YES(?,O(n^1))"
tttac :: T.Certificate -> TTTAC
tttac c@T.Certificate{}      = TTTAC (timebounds c)
tttac (T.CertificateYesNo x) = TTTACYN x

instance PP.Pretty TTTAC where
  pretty (TTTAC (Timebounds lb ub)) | lb /= T.Unknown || ub /= T.Unknown =
    PP.text "YES" <> PP.tupled [PP.pretty lb, PP.pretty ub]
  pretty (TTTACYN True ) = PP.text "YES"
  pretty (TTTACYN False) = PP.text "NO"
  pretty _               = PP.text "MAYBE"

instance Xml.Xml TTTAC where
  toXml (TTTAC (Timebounds lb ub)) | lb /= T.Unknown || ub /= T.Unknown =
    Xml.elt
      "certified"
      [Xml.elt "lowerbound" [Xml.toXml lb], Xml.elt "upperbound" [Xml.toXml ub]]
  toXml (TTTACYN True ) = Xml.elt "YES" []
  toXml (TTTACYN False) = Xml.elt "NO" []
  toXml _               = Xml.elt "maybe" []


--- * old termcomp format -------------------------------------------------------------------------------------------------------

-- | Newtype wrapper for 'Timebounds'.
-- The pretty printing instance corresponds to the /termcomp 2015/ format.
-- See <http://cbr.uibk.ac.at/competition/rules.php> (September 2015) for more information.
type Termcomp = DefaultFormat

-- | Returns the certificate in the /termcomp 2015/ format.
--
-- > pretty $ termcomp unbounded = "WORST_CASE(?,?)"
-- > pretty $ termcomp (timeLBCert linear) = "WORSTCASE(Omega(n^1,?)"
-- > pretty $ termcomp (timeUBCert linear) = "WORSTCASE(?,O(n^1)"
termcomp :: T.Certificate -> Termcomp
termcomp c@T.Certificate{}      = DefaultFormatWC (timebounds c)
termcomp (T.CertificateYesNo x) = DefaultFormatYN x

--- * default output format -------------------------------------------------------------------------------------------------------

-- | Newtype wrapper for 'Timebounds'.
-- The pretty printing instance corresponds to the default output format, including worst case and best case bounds.
-- See <http://cbr.uibk.ac.at/competition/rules.php> (September 2015) for more information.
data DefaultFormat = DefaultFormat Timebounds Timebounds
                   | DefaultFormatWC Timebounds
                   | DefaultFormatBC Timebounds
                   | DefaultFormatYN Bool

-- | Returns the certificate in the default format.
--
-- > pretty $ termcomp unbounded = "WORST_CASE(?,?)"
-- > pretty $ termcomp (timeLBCert linear) = "WORSTCASE(Omega(n^1,?)"
-- > pretty $ termcomp (timeUBCert linear) = "WORSTCASE(?,O(n^1)"
certificate2Default :: T.Certificate -> DefaultFormat
certificate2Default c@T.Certificate{} =
  DefaultFormat (timebounds c) (timeboundsBestCase c)
certificate2Default (T.CertificateYesNo x) = DefaultFormatYN x

toDefaultFormat
  :: (t -> t -> t)
  -> t
  -> ((t1, t1) -> t)
  -> ((t1, t1) -> t)
  -> (Int -> t1)
  -> t1
  -> (Int -> t1)
  -> t1
  -> t1
  -> DefaultFormat
  -> t
toDefaultFormat sep maybeA worst best omegaA npolyA oA polyA unknownA bounds =
  case bounds of
    DefaultFormat (Timebounds lb ub) (Timebounds bclb bcub) ->
      output worst (lb, ub) `sep` output best (bclb, bcub)
    DefaultFormatWC (Timebounds lb ub) -> output worst (lb, ub)
    DefaultFormatBC (Timebounds bclb bcub) -> output best (bclb, bcub)
    DefaultFormatYN _ -> error "This format should already be cached earlier."
 where
  output f (l, u) = case (normlb l, normub u) of
    (T.Unknown, T.Unknown) -> maybeA
    (nlb      , nub      ) -> f (toclb nlb, tocub nub)
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
toDefaultFormat _ _ _ _ _ _ _ _ _ _ = error "not possible"


instance PP.Pretty DefaultFormat where
  pretty (DefaultFormatYN True) = PP.text "YES"
  pretty (DefaultFormatYN False) = PP.text "NO"
  pretty defaultFormat = prettyDefaultFormatSep (PP.<$$>) defaultFormat


prettyDefaultFormatSep
  :: (PP.Doc -> PP.Doc -> PP.Doc) -> DefaultFormat -> PP.Doc
prettyDefaultFormatSep sep c = toDefaultFormat
  sep
  (PP.text "MAYBE")
  (\(lb, ub) -> PP.text "WORST_CASE" <> PP.tupled [lb, ub])
  (\(lb, ub) -> PP.text "BEST_CASE" <> PP.tupled [lb, ub])
  (\i -> PP.text "Omega" <> PP.parens (PP.text "n^" <> PP.int i))
  (PP.text "NON_POLY")
  (\i -> PP.char 'O'
    <> PP.parens (if i == 0 then PP.int 1 else PP.text "n^" <> PP.int i)
  )
  (PP.text "POLY")
  (PP.char '?')
  c

instance Xml.Xml DefaultFormat where
  toXml (DefaultFormatYN True ) = Xml.elt "YES" []
  toXml (DefaultFormatYN False) = Xml.elt "NO" []
  toXml c@DefaultFormat{}       = toDefaultFormat
    Xml.addChild
    (Xml.elt "maybe" [])
    (\(lb, ub) -> Xml.elt
      "worst_case"
      [Xml.elt "lowerbound" [lb], Xml.elt "upperbound" [ub]]
    )
    (\(lb, ub) -> Xml.elt
      "best_case"
      [Xml.elt "lowerbound" [lb], Xml.elt "upperbound" [ub]]
    )
    (\i -> Xml.elt "polynomial" [Xml.int i])
    (Xml.text "non_poly")
    (\i -> Xml.elt "polynomial" [Xml.int i])
    (Xml.text "poly")
    (Xml.text "unknown")
    c
