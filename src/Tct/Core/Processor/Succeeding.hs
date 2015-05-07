-- | This module provides the /Succeeding/ processors.
module Tct.Core.Processor.Succeeding
  (
  succeeding
  , succeedingDeclaration
  ) where


import           Tct.Core.Data


data Succeeding i = Succeeding deriving Show

instance ProofData i => Processor (Succeeding i) where
  type ProofObject (Succeeding i) = ()
  type I (Succeeding i)           = i
  type O (Succeeding i)           = i

  solve p prob = return . resultToTree p prob $ Success (toId prob) () fromId

-- | The succeeding strategy..
succeeding :: ProofData i => Strategy i i
succeeding = Proc (Succeeding :: ProofData i => Succeeding i)

-- | The succeeding strategy declaration.
succeedingDeclaration :: ProofData i => Declaration('[] :-> Strategy i i)
succeedingDeclaration = declare "succeeding" help () succeeding
  where help = ["This strategy always succeeds."]

