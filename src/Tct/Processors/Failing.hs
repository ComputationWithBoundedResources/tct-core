-- | This module implements the 'FailProcessor'.
-- This processor always fails.
module Tct.Processors.Failing
  ( 
  -- * StrategyDeclaration
  failingSD
  -- * Strategies
  , failing
  ) where


import qualified Tct.Common.Pretty as PP
import Tct.Core
import Tct.Core.Declaration.Parse as P ()

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
  declaration _                         = declareProcessor "failing" () FailProc
  solve p prob                          = return $ resultToTree p prob (Fail FailProof)

-- Default 'FailProcessor' instance.
failProcessor :: ProofData prob => FailProcessor prob
failProcessor = FailProc

-- | The failing strategy declaration.
failingSD :: ProofData prob => StrategyDeclaration prob
failingSD = SD . liftP $ declaration failProcessor

-- | The failing Strategy.
failing :: ProofData prob => Strategy prob
failing = Proc failProcessor

