-- | This module provides the /Cast/ processor. This processor can be used to cast a proof tree. Returns 'Halt' if the
-- proof tree is not closed.
module Tct.Core.Processor.Cast (cast, close) where


import Tct.Core.Data
import Tct.Core.Processor.Failing (failing)


data Cast i o where
  Cast :: ProofData j => Strategy i j -> Cast i o

instance Show (Cast i o) where
  show (Cast s) = "Cast " ++ show s

instance (ProofData i, Show o) => Processor (Cast i o) where
  type ProofObject (Cast i o) = ()
  type I (Cast i o)           = i
  type O (Cast i o)           = o

  solve (Cast st) prob = do
    ret <- evaluate st prob
    case ret of
      Halt pt  -> return $ Halt pt
      Abort pt -> return $ Halt (ProofBox `fmap` pt)
      Continue pt
        | isOpen pt -> return $ Halt (ProofBox `fmap` pt)
        | otherwise -> return $ Continue (undefined `fmap` pt)

cast :: (ProofData i, ProofData j, Show o) => Strategy i j -> Strategy i o
cast = Proc . castp where
  castp :: (ProofData i, ProofData j, Show o) => Strategy i j -> Cast i o
  castp = Cast

-- | Like 'cast' but is not a combinator. Example usage: Assume @s1 :: Strategy i o1@ and @s3 :: Strategy i o2@.
-- 
-- @
-- fastest [ s1 >=> s2 >>> close, s3 >=> s4  >>> close ] 
-- @
close :: (ProofData i, Show o) => Strategy i o
close = failing

