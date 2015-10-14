-- | This module provides the 'Processor' type.
-- 'Processor' instances define transformations from problems to a (possible empty) set of subproblems.
module Tct.Core.Data.Processor
  ( Processor (..)
  , Return (..)
  , Fork
  , ProofData
  , CertificateFn
  , apply
  , abortWith
  , succeedWith
  , succeedWith0
  , succeedWith1
  , succeedWithId
  ) where


import           Control.Monad.Error    (catchError)
import qualified Tct.Core.Common.Pretty as PP
import qualified Tct.Core.Data.Forks    as F
import           Tct.Core.Data.Types

apply :: Processor p => p -> In p -> TctM (ProofTree (Out p))
apply p i = toProofTree <$> (execute p i `catchError` handler)
  where
    toProofTree (NoProgress r)      = Failure r
    toProofTree (Progress pn cf ts) = Success (ProofNode p i pn) cf ts
    handler = return . NoProgress . IOError

succeedWith :: Processor p => ProofObject p -> CertificateFn p -> Forking p (Out p) -> TctM (Return p)
succeedWith pn cfn ts = return $ Progress pn cfn (Open <$> ts)

succeedWith0 :: (Processor p, Forking p ~ F.Judgement) => ProofObject p -> CertificateFn p -> TctM (Return p)
succeedWith0 pn cfn = return (Progress pn cfn F.Judgement)

succeedWith1 :: (Processor p, Forking p ~ F.Id) => ProofObject p -> CertificateFn p -> Out p -> TctM (Return p)
succeedWith1 pn cfn p = return $ Progress pn cfn (F.toId $ Open p)

succeedWithId :: (Processor p, Forking p ~ F.Id) => ProofObject p -> Out p -> TctM (Return p)
succeedWithId pn p = return $ Progress pn F.fromId (F.toId $ Open p)

abortWith :: (Show r, PP.Pretty r) => r -> TctM (Return p)
abortWith = return . NoProgress . SomeReason

