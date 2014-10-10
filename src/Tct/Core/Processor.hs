-- | This module provides the 'Processor' type.
-- 'Processor' instances define transformations from problems to a (possible empty) set of subproblems.
module Tct.Core.Processor 
  ( 
    Result (..)
  , Processor (..)
    
  , Fork
  , ProofData
  , CertificateFn
  , processorName
  , processorHelp
  , declareProcessor
  , resultToTree
    
  , ErroneousProcessor (..)
  , ErroneousProof (..)
  ) where

import           Tct.Core.Types
import qualified Tct.Common.Pretty as PP
import qualified Tct.Core.Declaration as Decl

-- Processor ---------------------------------------------------------------------------------------------------------

processorName :: Processor p => p -> String
processorName p = Decl.declName (declaration p)

processorHelp :: Processor p => p -> [String]
processorHelp p = Decl.declHelp (declaration p)

declareProcessor ::
  (ToHList a, HListOf a ~ ProcessorArgs p
  , f ~ Uncurry (ArgsType (ProcessorArgs p) :-> p)
  , p ~ Ret (ArgsType (ProcessorArgs p)) f)
  => String -> a -> f -> Declaration ((ProcessorArgs p) :-> p)
declareProcessor n as p = Decl n [] p (toHList as)

-- | Lifts the result of a 'Processor' application (see 'solve') to 'ProofTree'. Informally we have:
--
-- prop> 'Fail'    -> 'NoProgress'
-- prop> 'Success' -> 'Progress'
resultToTree :: Processor p => p -> Problem p -> Result p -> Return (ProofTree (Problem p))
resultToTree p prob (Fail po)                    = Abort $ NoProgress (ProofNode p prob po) (Open prob)
resultToTree p prob (Success subprobs po certfn) = Continue $ Progress (ProofNode p prob po) certfn (Open `fmap` subprobs)
              
-- Error Processor ---------------------------------------------------------------------------------------------------

data ErroneousProof p = ErroneousProof IOError p deriving Show

-- instance Processor p => Xml.Xml (ErroneousProof p) where
--   toXml (ErroneousProof err p) = 
--     Xml.elt "error" [] [ Xml.elt "processor" [] [Xml.text (name p)]
--                        , Xml.elt "message" [] [Xml.text (show err)] ]

instance Processor p => PP.Pretty (ErroneousProof p) where 
  pretty (ErroneousProof err p) = 
    PP.text "Processor" PP.<+> PP.squotes (PP.text (processorName p)) PP.<+> PP.text "signalled the following error:"
    PP.<$$> PP.indent 2 (PP.paragraph (show err))

data ErroneousProcessor p = ErroneousProc IOError p deriving Show

instance Processor p => Processor (ErroneousProcessor p) where
  type ProofObject (ErroneousProcessor p) = ErroneousProof p
  type Problem (ErroneousProcessor p)     = Problem p
  solve e@(ErroneousProc err p) prob      = return $ resultToTree e prob (Fail (ErroneousProof err p))
  declaration p                           = declareProcessor ("Erroneous processor" ++ processorName p) () p

