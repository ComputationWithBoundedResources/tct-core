-- | This module provides the standard answer type.
module Tct.Core.Data.Answer 
  ( Answer (..)
  , answer
  ) where

import           Tct.Core.Data.ProofTree
import qualified Tct.Core.Data.Certificate as T
import qualified Tct.Core.Common.Pretty as PP
import qualified Tct.Core.Common.Xml as Xml


-- | Standard answer type.
data Answer
  = CertAnswer (T.Complexity, T.Complexity)
  | MaybeAnswer
  | NoAnswer
  deriving Show

instance PP.Pretty Answer where
  pretty (CertAnswer (lb, ub)) 
    | lb /= T.Unknown || ub /= T.Unknown = PP.text "YES" PP.<> PP.tupled [PP.pretty lb, PP.pretty ub]
  pretty NoAnswer = PP.text "NO"
  pretty _        = PP.text "MAYBE"

instance Xml.Xml Answer where
  toXml (CertAnswer (lb, ub)) 
    | lb /= T.Unknown || ub /= T.Unknown = Xml.elt "certified" 
      [ Xml.elt "lowerbound" [Xml.toXml lb]
      , Xml.elt "upperbound" [Xml.toXml ub] ]
  toXml NoAnswer = Xml.elt "no" []
  toXml _        = Xml.elt "maybe" []

-- | Returns the time upper bound as an answer.
answer :: ProofTree l -> Answer
answer = cert . certificate
  where cert c = CertAnswer (T.timeLB c, T.timeUB c)

