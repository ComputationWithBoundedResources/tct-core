-- | This module provides the /Cast/ processor.
-- This processor can be used to cast a proof tree.
-- Returns 'Flop' if the proof tree is not closed.
module Tct.Core.Processor.Cast (cast, close) where


import Tct.Core.Data
import Tct.Core.Processor.Failing (failing)


data Cast i o where
  Cast :: Strategy i j -> Cast i o

instance Show (Cast i o) where
  show (Cast s) = "Cast: " ++ show s

instance (ProofData i, Show o) => Processor (Cast i o) where
  type ProofObject (Cast i o) = ()
  type I (Cast i o) = i
  type O (Cast i o) = o

  solve (Cast st) prob = do
    ret <- evaluate st prob
    case ret of
      Flop    -> return Flop
      Abort _ -> return Flop
      Continue pt
        | isOpen pt -> return Flop
        | otherwise -> return $ Continue (undefined `fmap` pt)

cast :: (ProofData i, Show o) => Strategy i j -> Strategy i o
cast = Proc . castp where
  castp :: (ProofData i, Show o) => Strategy i j -> Cast i o
  castp = Cast

-- TODO: MS: test this: this should actually already be enough to eg cast in alternatives
close :: (ProofData i, Show o) => Strategy i o
close = failing

