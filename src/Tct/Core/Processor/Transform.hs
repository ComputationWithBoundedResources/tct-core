-- | This module provides a simple /Transform/ processor with a minimal proof output.
module Tct.Core.Processor.Transform (transform) where


import qualified Tct.Core.Common.Pretty          as PP
import qualified Tct.Core.Common.Xml             as Xml
import           Tct.Core.Data


data Transform i o = Transform (i -> Return o)

instance Show (Transform i o) where
  show _ = "Problem Transformation."

data TransformProof i o
  = TransformProof i o
  | TransformFail
  deriving Show

instance (ProofData i, ProofData o) => Processor (Transform i o) where
  type ProofObject (Transform i o) = TransformProof i o
  type I (Transform i o)           = i
  type O (Transform i o)           = o

  solve p@(Transform t) prob = t prob `seq` return . resultToTree' p prob $ case t prob of
    Halt _         -> Fail TransformFail
    Abort _        -> Fail TransformFail
    Continue nprob -> Success (toId nprob) (TransformProof prob nprob) fromId


-- | A Wrapper for a transfromation. With minimal proof output.
transform :: (ProofData i, ProofData o) => (i -> Return o) -> Strategy i o
transform = Proc . Transform


instance (PP.Pretty i, PP.Pretty o) => PP.Pretty (TransformProof i o) where
  pretty TransformFail        = PP.text "The transformation failed."
  pretty (TransformProof i o) = PP.vcat
    [ PP.text "We transform the problem"
    , PP.indent 2 $ PP.pretty i
    , PP.text "into the problem"
    , PP.indent 2 $ PP.pretty o ]

instance (Xml.Xml i, Xml.Xml o) => Xml.Xml (TransformProof i o) where
  toXml TransformFail        = Xml.elt "transformation" [ Xml.elt "failed" []]
  toXml (TransformProof i o) =
    Xml.elt "transformation"
      [ Xml.toXml i
      , Xml.toXml o ]

