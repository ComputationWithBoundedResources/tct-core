-- | This module provides the standard answer type.
module Tct.Core.Data.Answer
  ( DefaultAnswer (..)
  , defaultAnswer
  , CompetitionAnswer (..)
  , competitionAnswer
  ) where


import           Data.Monoid               ((<>))
import qualified Tct.Core.Common.Pretty    as PP
import qualified Tct.Core.Common.Xml       as Xml

import qualified Tct.Core.Data.Certificate as T
import           Tct.Core.Data.ProofTree


-- | Default answer type.
data DefaultAnswer
  = CertDefaultAnswer (T.Complexity, T.Complexity)
  | MaybeDefaultAnswer
  | NoDefaultAnswer
  deriving Show

instance PP.Pretty DefaultAnswer where
  pretty (CertDefaultAnswer (lb, ub))
    | lb /= T.Unknown || ub /= T.Unknown = PP.text "YES" <> PP.tupled [PP.pretty lb, PP.pretty ub]
  pretty NoDefaultAnswer = PP.text "NO"
  pretty _        = PP.text "MAYBE"

instance Xml.Xml DefaultAnswer where
  toXml (CertDefaultAnswer (lb, ub))
    | lb /= T.Unknown || ub /= T.Unknown = Xml.elt "certified"
      [ Xml.elt "lowerbound" [Xml.toXml lb]
      , Xml.elt "upperbound" [Xml.toXml ub] ]
  toXml NoDefaultAnswer = Xml.elt "no" []
  toXml _        = Xml.elt "maybe" []

-- | Returns the time upper bound as an answer.
defaultAnswer :: ProofTree l -> DefaultAnswer
defaultAnswer = cert . certificate
  where cert c = CertDefaultAnswer (T.timeLB c, T.timeUB c)

-- | Competition answer type.
data CompetitionAnswer = CertCompetitionAnswer (T.Complexity, T.Complexity)
  deriving Show

toCompetitionAnswer :: t -> ((t1, t1) -> t) -> (Int -> t1) -> t1 -> (Int -> t1) -> t1 -> t1 -> CompetitionAnswer -> t
toCompetitionAnswer maybeA worst omegaA npolyA oA polyA unknownA (CertCompetitionAnswer (lb, ub)) = case (normlb lb, normub ub) of
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

instance PP.Pretty CompetitionAnswer where
  pretty =
    toCompetitionAnswer
      (PP.text "MAYBE")
      (\(lb,ub) -> PP.text "WORST_CASE" <> PP.tupled [lb,ub])
      (\i -> PP.text "Omega" <> PP.parens (PP.text "n^" <> PP.int i))
      (PP.text "NON_POLY")
      (\i -> PP.char 'O' <> PP.parens (if i == 0 then PP.int 1 else PP.text "n^" <> PP.int i))
      (PP.text "POLY")
      (PP.char '?')

instance Xml.Xml CompetitionAnswer where
  toXml =
    toCompetitionAnswer
      (Xml.elt "maybe" [])
      (\(lb,ub) -> Xml.elt "worst_case" [Xml.elt "lowerbound" [lb], Xml.elt "upperbound" [ub]])
      (\i -> Xml.elt "polynomial" [Xml.int i])
      (Xml.text "non_poly")
      (\i -> Xml.elt "polynomial" [Xml.int i])
      (Xml.text "poly")
      (Xml.text "unknown")

-- | Returns the answer in the termcomp format.
competitionAnswer :: ProofTree l -> CompetitionAnswer
competitionAnswer = cert . certificate
  where cert c = CertCompetitionAnswer (T.timeLB c, T.timeUB c)

