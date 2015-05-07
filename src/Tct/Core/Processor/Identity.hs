-- | This module provides the /Identity/ processors.
-- Similar to 'Failing' but returns 'Abort'.
module Tct.Core.Processor.Identity
  (
  identity
  , identityDeclaration
  ) where


import qualified Tct.Core.Common.Pretty          as PP
import qualified Tct.Core.Common.Xml             as Xml
import           Tct.Core.Data                   hiding (timed)


data Identity i = Identity deriving Show

data IdentityProof = IdentityProof deriving Show

instance ProofData i => Processor (Identity i) where
  type ProofObject (Identity i) = IdentityProof
  type I (Identity i)           = i
  type O (Identity i)           = i

  solve p prob = return . resultToTree p prob $ Fail IdentityProof


-- | The identity strategy. Always Fails.
identity :: ProofData i => Strategy i i
identity = Proc (Identity :: (ProofData i) => Identity i)

-- | The identity strategy declaration.
identityDeclaration :: ProofData i => Declaration('[] :-> Strategy i i)
identityDeclaration = declare "identity" help () identity
  where help = ["This strategy always fails."]

instance PP.Pretty IdentityProof where
  pretty IdentityProof    = PP.text "The identity transformation. No Progress."

instance Xml.Xml IdentityProof where
  toXml IdentityProof    = Xml.elt "identity" []

