-- | This module provides some /Simple/ processors.
module Tct.Core.Processor.Simple
  (
  -- * Failed
  failing
  , failingWith
  , failingWithDeclaration
  -- * Identity
  , identity
  , identityDeclaration
  -- * Succeed
  , succeeding
  , succeedingDeclaration
  -- * Annotations
  , named
  , timed
  ) where

import           Control.Monad.Trans             (liftIO)
import qualified System.Time                     as Time

import qualified Tct.Core.Common.Pretty          as PP
import           Tct.Core.Common.SemiRing
import qualified Tct.Core.Common.Xml             as Xml
import           Tct.Core.Data                   hiding (timed)
import           Tct.Core.Data.Declaration.Parse as P ()


data Simple prob
  = Simple (SimpleProof) deriving Show

data SimpleProof
  = Failed String
  | Identity
  | Succeeded
  deriving Show

instance PP.Pretty SimpleProof where
  pretty (Failed []) = PP.text "Fail."
  pretty (Failed xs) = PP.text "Fail. The reason is:" PP.<+> PP.text xs PP.<> PP.dot
  pretty Identity    = PP.text "The identity transformation. No Progress."
  pretty Succeeded   = PP.text "Success."

instance Xml.Xml SimpleProof where
  toXml (Failed []) = Xml.elt "failed" []
  toXml (Failed xs) = Xml.elt "failed" [Xml.text xs]
  toXml Identity    = Xml.elt "identity" []
  toXml Succeeded   = Xml.elt "success" [] 

instance ProofData prob => Processor (Simple prob) where
  type ProofObject (Simple prob) = SimpleProof
  type Problem (Simple prob)     = prob
  solve p@(Simple t) prob = return . resultToTree p prob $ case t of
    Failed xs  -> Fail (Failed xs)
    Identity   -> Fail (Identity)
    Succeeded  -> Success (Id prob) Succeeded bigAdd


-- | The failing Strategy.
failing :: ProofData prob => Strategy prob
failing = failingWith ""

-- | The failing Strategy.
failingWith :: ProofData prob => String -> Strategy prob
failingWith xs = Proc (Simple (Failed xs) :: ProofData prob => Simple prob)

-- | The failing Strategy declaration.
failingWithDeclaration :: ProofData prob => Declaration('[ Argument 'Optional String] :-> Strategy prob)
failingWithDeclaration = declare "failingWith" [help] (OneTuple $ msg) failingWith
  where 
    help = "This strategy always fails. Does not abort compuatation in combination with try."
    msg =  string `withHelp` ["The failing message."] `optional` ""

-- | The identity strategy. Always fails.
identity :: ProofData prob => Strategy prob
identity = Proc (Simple Identity :: ProofData prob => Simple prob)

-- | The succeeding strategy declaration.
identityDeclaration :: ProofData prob => Declaration('[] :-> Strategy prob)
identityDeclaration = declare "identity" [help] () identity
  where help = "This strategy always fails."

-- | The succeeding strategy..
succeeding :: ProofData prob => Strategy prob
succeeding = Proc (Simple Succeeded :: ProofData prob => Simple prob)

-- | The succeeding strategy declaration.
succeedingDeclaration :: ProofData prob => Declaration('[] :-> Strategy prob)
succeedingDeclaration = declare "succeeding" [help] () succeeding
  where help = "This strategy always succeeds."

data Annotation prob
  = Timed (Strategy prob)
  | Named String (Strategy prob)
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

instance ProofData prob => Processor (Annotation prob) where
  type ProofObject (Annotation prob) = AnnotationProof
  type Problem (Annotation prob)     = prob
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

named :: ProofData prob => String -> Strategy prob -> Strategy prob
named n = Proc . Named n

timed :: ProofData prob => Strategy prob -> Strategy prob
timed = Proc . Timed

