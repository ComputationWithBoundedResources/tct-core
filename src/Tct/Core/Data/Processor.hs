-- | This module provides the 'Processor' type.
-- 'Processor' instances define transformations from problems to a (possible empty) set of subproblems.
module Tct.Core.Data.Processor 
  ( 
    Result (..)
  , Processor (..)
  , toStrategy
    
  , Fork
  , ProofData
  , CertificateFn
  , resultToTree
  , resultToTreeF

  , ErroneousProcessor (..)
  , ErroneousProof (..)
  ) where


import qualified Tct.Core.Common.Pretty    as PP
import qualified Tct.Core.Common.Xml       as Xml
import           Tct.Core.Data.Types


toStrategy :: Processor p => p -> Strategy (I p) (O p)
toStrategy = Proc

-- Processor ---------------------------------------------------------------------------------------------------------

-- | Lifts the result of a 'Processor' application (see 'solve') to 'ProofTree'. Informally we have:
--
-- prop> 'Fail'    -> 'NoProgress'
-- prop> 'Success' -> 'Progress'
resultToTree :: (Processor p, I p ~ O p) => p -> I p -> Result p -> Return (ProofTree (O p))
resultToTree p prob (Success probs po certfn) = Continue $ Progress (ProofNode p prob po) certfn (Open `fmap` probs)
resultToTree p prob (Fail po)                 = Abort $ NoProgress (ProofNode p prob po) (Open prob)

-- prop> 'Fail'    -> 'NoProgress'
-- prop> 'Success' -> 'Progress'
resultToTreeF :: Processor p => p -> I p -> Result p -> Return (ProofTree (O p))
resultToTreeF p prob (Success probs po certfn) = Continue $ Progress (ProofNode p prob po) certfn (Open `fmap` probs)
resultToTreeF _ _ (Fail _)                     = Flop

-- Error Processor ---------------------------------------------------------------------------------------------------

data ErroneousProof p = ErroneousProof IOError p deriving Show

instance Processor p => PP.Pretty (ErroneousProof p) where 
  pretty (ErroneousProof err p) = 
    PP.text "Processor" PP.<+> PP.squotes (PP.text (show p)) PP.<+> PP.text "signalled the following error:"
    PP.<$$> PP.indent 2 (PP.paragraph (show err))

instance Processor p => Xml.Xml (ErroneousProof p) where
  toXml (ErroneousProof err p) = Xml.elt "error" 
    [ Xml.elt "processor" [Xml.text $ show p]
    , Xml.elt "message"   [Xml.text $ show err] ] 

data ErroneousProcessor p = ErroneousProc IOError p deriving Show

instance Processor p => Processor (ErroneousProcessor p) where
  type ProofObject (ErroneousProcessor p) = ErroneousProof p
  type I (ErroneousProcessor p)     = I p
  type O (ErroneousProcessor p)     = O p

  solve e@(ErroneousProc err p) prob      = return . resultToTreeF e prob $ Fail (ErroneousProof err p)

