-- | This module provides the /Assumption/ processors.
-- Though generally not sound, it can be useful to close a node of a tree.
module Tct.Core.Processor.Assumption (assumeWith) where


import Tct.Core.Common.SemiRing
import Tct.Core.Data


data Assumption prob = Assumption
  deriving Show

instance ProofData prob => Processor (Assumption prob) where
  type ProofObject (Assumption prob) = ()
  type Problem (Assumption prob)     = prob
  type Forking (Assumption prob)     = Judgement

  solve p prob = return . resultToTree p prob $ Success Judgement () (judgement zero)

closeNode :: ProofData prob => Certificate -> Assumption prob -> prob -> ProofTree prob
closeNode cert as prob = Progress pn (judgement cert) Judgement where
  pn = ProofNode
    { processor = as
    , problem   = prob
    , proof     = () }

assumeWith :: ProofData prob => Certificate -> ProofTree prob -> ProofTree prob
assumeWith cert (Open l)                 = closeNode cert Assumption l
assumeWith cert (NoProgress pn pt)       = NoProgress pn (assumeWith cert pt)
assumeWith cert (Progress pn certfn pts) = Progress pn certfn (assumeWith cert `fmap` pts)

