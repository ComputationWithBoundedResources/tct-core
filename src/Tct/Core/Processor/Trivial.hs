-- | This module provides some trivial processors.
module Tct.Core.Processor.Trivial
  ( 
  -- * Failed
  failingWithDeclaration
  , failingWith
  , failing
  -- * Identity
  , identity
  , identityDeclaration
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
  = Failed String
  | Identity
  | Succeeded
  deriving Show


instance PP.Pretty TrivialProof where
  pretty (Failed []) = PP.text "Fail."
  pretty (Failed xs) = PP.text "Fail. The reason is:" PP.<+> PP.text xs PP.<> PP.dot
  pretty Identity    = PP.text "The identity transformation. No Progress."
  pretty Succeeded   = PP.text "Success."

instance ProofData prob => Processor (TrivialProcessor prob) where
  type ProofObject (TrivialProcessor prob) = TrivialProof
  type Problem (TrivialProcessor prob)     = prob
  solve p@(TrivialProc t) prob = return . resultToTree p prob $ case t of
    Failed xs  -> Fail (Failed xs)
    Identity   -> Fail (Identity)
    Succeeded  -> Success (Id prob) Succeeded bigAdd


-- TODO: combinators are phantom types; we need explicit type signature
-- can we do better

-- | The failing Strategy.
failing :: ProofData prob => Strategy prob
failing = failingWith ""

-- | The failing Strategy.
failingWith :: ProofData prob => String -> Strategy prob
failingWith xs = Proc (TrivialProc (Failed xs) :: ProofData prob => TrivialProcessor prob)

-- | The failing Strategy declaration.
failingWithDeclaration :: ProofData prob => Declaration('[ Argument 'Optional String] :-> Strategy prob)
failingWithDeclaration = declare "failingWith" [help] (OneTuple $ msg) failingWith
  where 
    help = "This strategy always fails. Does not abort compuatation in combination with try."
    msg =  string `withHelp` ["The failing message."] `optional` ""

-- | The identity strategy. Always fails.
identity :: ProofData prob => Strategy prob
identity = Proc (TrivialProc Identity :: ProofData prob => TrivialProcessor prob)

-- | The succeeding strategy declaration.
identityDeclaration :: ProofData prob => Declaration('[] :-> Strategy prob)
identityDeclaration = declare "identity" [help] () identity
  where help = "This strategy always fails."

-- | The succeeding strategy..
succeeding :: ProofData prob => Strategy prob
succeeding = Proc (TrivialProc Succeeded :: ProofData prob => TrivialProcessor prob)

-- | The succeeding strategy declaration.
succeedingDeclaration :: ProofData prob => Declaration('[] :-> Strategy prob)
succeedingDeclaration = declare "succeeding" [help] () succeeding
  where help = "This strategy always succeeds."

