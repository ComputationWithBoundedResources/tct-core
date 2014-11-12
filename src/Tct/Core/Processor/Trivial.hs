-- | This module provides some trivial processors.
module Tct.Core.Processor.Trivial
  ( 
  -- * Failed
  failingDeclaration
  , failing
  -- * Succeed
  , succeedingDeclaration
  , succeeding
  ) where


import           Tct.Core.Common.SemiRing        as PP
import qualified Tct.Core.Common.Pretty          as PP
import           Tct.Core.Data
import           Tct.Core.Data.Declaration.Parse as P ()


data TrivialProcessor prob
  = TrivialProc (TrivialProof) deriving Show

data TrivialProof
  = Failed
  | Succeeded
  deriving Show


instance PP.Pretty TrivialProof where
  pretty Failed    = PP.text "Fail."
  pretty Succeeded = PP.text "Success."

instance ProofData prob => Processor (TrivialProcessor prob) where
  type ProofObject (TrivialProcessor prob) = TrivialProof
  type Problem (TrivialProcessor prob)     = prob
  solve p@(TrivialProc t) prob = return . resultToTree p prob $ case t of
    Failed    -> Fail Failed
    Succeeded -> Success (Id prob) Succeeded bigAdd


-- TODO: combinators are phantom types; we need explicit type signature
-- can we do better

-- | The failing Strategy.
failing :: ProofData prob => Strategy prob
failing = Proc (TrivialProc Failed :: ProofData prob => TrivialProcessor prob)

-- | The failing Strategy declaration.
failingDeclaration :: ProofData prob => Declaration('[] :-> Strategy prob)
failingDeclaration = declare "failing" [help] () failing
  where help = "This strategy always fails. Does not abort compuatation in combination with try."


-- | The succeeding strategy..
succeeding :: ProofData prob => Strategy prob
succeeding = Proc (TrivialProc Succeeded :: ProofData prob => TrivialProcessor prob)

-- | The succeeding strategy declaration.
succeedingDeclaration :: ProofData prob => Declaration('[] :-> Strategy prob)
succeedingDeclaration = declare "succeeding" [help] () succeeding
  where help = "This strategy always succeeds."

