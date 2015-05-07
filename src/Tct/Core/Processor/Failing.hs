-- | This module provides the /Failing/ processors.
-- Always returns 'Stop'. In Combination with 'try' computation does not stop.
module Tct.Core.Processor.Failing
  (
  failing
  , failing'
  , failingDeclaration
  ) where


import qualified Tct.Core.Common.Pretty          as PP
import qualified Tct.Core.Common.Xml             as Xml
import           Tct.Core.Data                   hiding (timed)


data Failing i o = Failing String deriving Show

data FailingProof = FailingProof String deriving Show

instance (ProofData i, Show o) => Processor (Failing i o) where
  type ProofObject (Failing i o) = FailingProof
  type I (Failing i o)           = i
  type O (Failing i o)           = o

  solve p@(Failing t) prob = return . resultToTree' p prob $ Fail (FailingProof t)

instance PP.Pretty FailingProof where
  pretty (FailingProof []) = PP.text "Fail."
  pretty (FailingProof xs) = PP.text "Fail. The reason is:" PP.<+> PP.text xs PP.<> PP.dot

instance Xml.Xml FailingProof where
  toXml (FailingProof []) = Xml.elt "failed" []
  toXml (FailingProof xs) = Xml.elt "failed" [Xml.text xs]

-- | The failing Strategy.
failing :: (ProofData i, Show o) => Strategy i o
failing = deflFun failingDeclaration

-- | The failing Strategy. Adds given string to the proof output.
failing' :: (ProofData i, Show o) => String -> Strategy i o
failing' = declFun failingDeclaration

-- | The failing Strategy declaration.
failingDeclaration :: (ProofData i, Show o) => Declaration('[ Argument 'Optional String] :-> Strategy i o)
failingDeclaration = declare "failing" help (OneTuple $ msg) p
  where
    help = ["This strategy always fails. Does not abort compuatation in combination with try."]
    msg =  string `withHelp` ["The failing message."] `optional` ""
    p s = Proc (Failing s :: (ProofData i, Show o) => Failing i o)

