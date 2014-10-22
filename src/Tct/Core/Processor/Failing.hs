-- | This module implements the 'FailProcessor'.
-- This processor always fails.
module Tct.Core.Processor.Failing
  ( 
  -- * StrategyDeclaration
  failingDeclaration
  -- * Strategies
  , failing
  ) where


import qualified Tct.Core.Common.Pretty          as PP
import           Tct.Core.Data
import           Tct.Core.Data.Declaration.Parse as P ()


data FailProof = FailProof deriving Show

instance PP.Pretty FailProof where
  pretty _ = PP.paragraph "We apply FailProccessor."

{-instance Xml.Xml FailProof where-}
  {-toXml _ = Xml.elt "fail" []-}

-- | A processor that always fails.
data FailProcessor prob = FailProc deriving Show

instance ProofData prob => Processor (FailProcessor prob) where
  type ProofObject (FailProcessor prob) = FailProof
  type Forking (FailProcessor prob)     = Judgement
  type Problem (FailProcessor prob)     = prob
  declaration p                         = declareProcessor "failing" ["Processor 'failing' always fails."] () (Proc p)
  solve p prob                          = return $ resultToTree p prob (Fail FailProof)

-- Default 'FailProcessor' instance.
failingProcessor :: ProofData prob => FailProcessor prob
failingProcessor = FailProc

-- | The failing Strategy declaration.
failingDeclaration :: ProofData prob => Declaration('[] :-> Strategy prob)
failingDeclaration = declaration failingProcessor


-- | The failing Strategy.
failing :: ProofData prob => Strategy prob
failing = Proc failingProcessor

