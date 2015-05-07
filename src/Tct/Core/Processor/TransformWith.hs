-- | This module provides the /TransformWith/ processor with a minimal proof output.
-- Similar to 'Transform' but applies the underlying strategy.
-- Applies 'close' and checks if the proof tree is closed.
module Tct.Core.Processor.TransformWith (transformWith) where


import qualified Tct.Core.Common.Pretty  as PP
import qualified Tct.Core.Common.Xml     as Xml
import           Tct.Core.Data
import           Tct.Core.Processor.Cast (close)


data TransformWith i where
  TransformWith :: (ProofData o, ProofData p) => (i -> Return o) -> Strategy o p -> TransformWith i

instance Show (TransformWith i) where
  show _ = "Problem Transformation."

data TransformWithProof i
  = TransformWithProof i
  | TransformWithFail
  deriving Show

instance ProofData i => Processor (TransformWith i) where
  type ProofObject (TransformWith i) = TransformWithProof i
  type I (TransformWith i)           = i
  type O (TransformWith i)           = i

  solve p@(TransformWith t st) prob = t prob `seq` case t prob of
    Halt _         -> return . resultToTree p prob $ Fail TransformWithFail
    Abort _        -> return . resultToTree p prob $ Fail TransformWithFail
    Continue nprob -> evaluate (st `Trans` close) nprob

transformWith :: (ProofData i, ProofData o, ProofData p) => (i -> Return o) -> Strategy o p -> Strategy i i
transformWith t = Proc . TransformWith t

instance PP.Pretty i => PP.Pretty (TransformWithProof i) where
  pretty TransformWithFail  = PP.text "The transformation failed."
  pretty (TransformWithProof i) = PP.vcat
    [ PP.text "We transform the problem"
    , PP.indent 2 $ PP.pretty i ]

instance Xml.Xml i => Xml.Xml (TransformWithProof i) where
  toXml TransformWithFail      = Xml.elt "transformationWith" [ Xml.elt "failed" []]
  toXml (TransformWithProof i) = Xml.elt "transformationWith" [ Xml.toXml i ]

