-- | This module provides the /Annotated/ processors.
module Tct.Core.Processor.Annotated
  (
  named
  , timed
  ) where

import           Control.Monad.Trans             (liftIO)
import qualified System.Time                     as Time

import qualified Tct.Core.Common.Pretty          as PP
import qualified Tct.Core.Common.Xml             as Xml
import           Tct.Core.Data                   hiding (timed)
import           Tct.Core.Data.Declaration.Parse as P ()


data Annotation i o
  = Timed (Strategy i o)
  | Named String (Strategy i o)
  deriving Show

data AnnotationProof
  = TimedProof Double
  | NamedProof String
  deriving Show

instance PP.Pretty AnnotationProof where
  pretty = PP.text . show

instance Xml.Xml AnnotationProof where
  toXml (TimedProof d) = Xml.elt "timed" [Xml.text $ show d]
  toXml (NamedProof n) = Xml.elt "named" [Xml.text n]

instance ProofData i => Processor (Annotation i o) where
  type ProofObject (Annotation i o) = AnnotationProof
  type I (Annotation i o) = i
  type O (Annotation i o) = o

  solve p@(Timed st) prob = do
    t1 <- liftIO Time.getClockTime
    ret <- evaluate st prob
    t2 <- liftIO Time.getClockTime
    let
      diff = fromIntegral (Time.tdPicosec (Time.diffClockTimes t2 t1)) / (10**(-12))
      pn = ProofNode { processor = p, problem = prob, proof = TimedProof (diff :: Double) }
    return $  NoProgress pn `fmap` ret
  solve p@(Named n st) prob = do
    ret <- evaluate st prob
    let pn = ProofNode {processor = p, problem = prob, proof = NamedProof n}
    return $  NoProgress pn `fmap` ret

named :: ProofData i => String -> Strategy i o -> Strategy i o
named n = Proc . Named n

timed :: ProofData i => Strategy i o -> Strategy i o
timed = Proc . Timed

