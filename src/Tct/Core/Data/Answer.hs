-- | This module provides the standard answer type.
module Tct.Core.Data.Answer where

import           Tct.Core.Data.ProofTree
import qualified Tct.Core.Data.Certificate as T
import qualified Tct.Core.Common.Pretty as PP


-- | Standard answer type.
data Answer
  = Yes (T.Complexity, T.Complexity)
  | Unknown
  | No 
  deriving Show

instance PP.Pretty Answer where
  pretty (Yes (lb, ub)) 
    | lb /= T.Unknown || ub /= T.Unknown = PP.text "YES" PP.<> PP.tupled [PP.pretty lb, PP.pretty ub]
  pretty No = PP.text "NO"
  pretty _  = PP.text "MAYBE"

-- | Returns the time upper bound as an answer.
answering :: ProofTree l -> Answer
answering = cert . certificate
  where cert c = Yes (T.timeLB c, T.timeUB c)

