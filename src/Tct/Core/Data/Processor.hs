-- | This module provides the 'Processor' type.
-- 'Processor' instances define transformations from problems to a (possible empty) set of subproblems.
module Tct.Core.Data.Processor
  ( Processor (..)

  , Fork
  , ProofData
  , CertificateFn

  , toStrategy
  , apply 
  -- , ErroneousProof (..)
  -- , solveCatchingIOErr
  ) where



import qualified Tct.Core.Common.Xml    as Xml
import           Tct.Core.Data.Types
import           Control.Monad.Error     (catchError)

--- * Processor ------------------------------------------------------------------------------------------------------

-- | prop> toStrategy == Proc
toStrategy :: Processor p => p -> Strategy (I p) (O p)
toStrategy = Apply

apply :: Processor p => p -> I p -> TctM (ProofTree (O p))
apply p i = do 
   res <- solve p i `catchError` handler
   return (toProofTree res)
  where 
    toProofTree Failure = Fail
    toProofTree (Progress ob ts c) = Success (ProofNode p i ob) ts c
    handler _ = return Failure
-- --- * Error Processor ------------------------------------------------------------------------------------------------

-- data ErroneousProof p = ErroneousProof IOError p deriving Show

-- instance Processor p => PP.Pretty (ErroneousProof p) where
--   pretty (ErroneousProof err p) = PP.vcat
--     [ PP.text "Processor" PP.<+> PP.squotes (PP.text (show p)) PP.<+> PP.text "signalled the following error:"
--     , PP.indent 2 $ PP.paragraph (show err) ]

-- instance Processor p => Xml.Xml (ErroneousProof p) where
--   toXml (ErroneousProof err p) = Xml.elt "error"
--     [ Xml.elt "processor" [Xml.text $ show p]
--     , Xml.elt "message"   [Xml.text $ show err] ]

-- data ErroneousProcessor p = ErroneousProc IOError p deriving Show

-- instance Processor p => Processor (ErroneousProcessor p) where
--   type ProofObject (ErroneousProcessor p) = ErroneousProof p
--   type I (ErroneousProcessor p)      = I p
--   type O (ErroneousProcessor p)      = O p

--   solve ep@(ErroneousProc e p) prob = failWith ep prob (ErroneousProof e p)

-- solveCatchingIOErr :: Processor p => p -> I p -> TctM (ProofTree (O p))
-- solveCatchingIOErr p prob = solve p prob `catchError` const (return Fail)
