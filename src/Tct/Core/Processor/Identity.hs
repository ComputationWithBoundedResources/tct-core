-- | This module provides the /Identity/ processors.
module Tct.Core.Processor.Identity
  (
  identity
  , identityDeclaration
  ) where


import qualified Tct.Core.Common.Pretty          as PP
import qualified Tct.Core.Common.Xml             as Xml
import           Tct.Core.Data                   hiding (timed)
import           Tct.Core.Data.Declaration.Parse as P ()


data Identity i o = Identity deriving Show

data IdentityProof = IdentityProof deriving Show

instance (ProofData i, Show o) => Processor (Identity i o) where
  type ProofObject (Identity i o) = IdentityProof
  type I (Identity i o)           = i
  type O (Identity i o)           = o

  solve p prob = return . resultToTreeF p prob $ Fail IdentityProof


-- | The identity strategy. Always Fails.
identity :: (ProofData i, Show o) => Strategy i o
identity = Proc (Identity :: (ProofData i, Show o) => Identity i o )

-- | The identity strategy declaration.
identityDeclaration :: (ProofData i, Show o) => Declaration('[] :-> Strategy i o)
identityDeclaration = declare "identity" help () identity
  where help = ["This strategy always fails."]

instance PP.Pretty IdentityProof where
  pretty IdentityProof    = PP.text "The identity transformation. No Progress."

instance Xml.Xml IdentityProof where
  toXml IdentityProof    = Xml.elt "identity" []

