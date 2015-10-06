-- | This module provides the 'Strategy' type.
module Tct.Core.Data.Strategy
  (
  Strategy (..)
  -- * Strategy evaluation
  , evaluate
  , evaluateTree
  , evaluateTreePar
  -- * Declaration
  , StrategyDeclaration (..)
  , strategy
  ) where

-- import Debug.Trace

import           Data.Foldable           as F
import           Data.Traversable        as T

import qualified Tct.Core.Common.Pretty  as PP

import           Tct.Core.Data.Processor
import           Tct.Core.Data.ProofTree
import           Tct.Core.Data.TctM
import           Tct.Core.Data.Types


instance Show (Strategy i o) where
  show _ = "someStrategy"

instance PP.Pretty (Strategy i o) where
  pretty _ = PP.text "someStrategy"


pickNonFail :: ProofTree o -> ProofTree o -> ProofTree o
pt1@Success{} `pickNonFail` _             = pt1
_             `pickNonFail` pt2@Success{} = pt2
pt1@Open{}    `pickNonFail` _             = pt1
_             `pickNonFail` pt2@Open{}    = pt2
pt1           `pickNonFail` _             = pt1

-- | @'evaluate' s prob@ defines the application of @s@ to a problem.
-- See "Combinators" for a detailed description.
evaluate :: ProofData o => Strategy i o -> i -> TctM (ProofTree o)
evaluate (Proc p) prob = solveCatchingIOErr p prob
evaluate (Trans s1 s2) prob = evaluate s1 prob >>= evaluateTree s2 
evaluate (Try s) prob = do
  r1 <- evaluate s prob
  return $ case r1 of
    Fail -> Open prob
    pt -> pt
evaluate (Force s) prob = do
  r1 <- evaluate s prob
  return $ case r1 of
    Open _ -> Fail
    pt -> pt
evaluate (WithStatus f) prob = do
  st <- askStatus prob
  evaluate (f st) prob
evaluate (WithState f s) prob =
  setState f (evaluate s prob)
evaluate (Alt s1 s2) prob = do
  pt1 <- evaluate s1 prob
  case pt1 of
    Success {} -> return pt1
    _ -> do
      pt2 <- evaluate s2 prob
      return (pickNonFail pt1 pt2)
evaluate (OrFaster s1 s2) prob =
  raceWith nonFail pickNonFail (evaluate s1 prob) (evaluate s2 prob) where
    nonFail Fail = False
    nonFail _    = True
evaluate (OrBetter cmp s1 s2) prob = do
  uncurry pickNonFail <$> concurrently (evaluate s1 prob) (evaluate s2 prob)

-- | 'evaluate' on a 'ProofTree'.
evaluateTree :: ProofData o => Strategy i o -> ProofTree i -> TctM (ProofTree o)
evaluateTree s (Open p)  = evaluate s p
evaluateTree s Fail      = return Fail
evaluateTree s (Success pn certfn subtrees) =
  Success pn certfn <$> (evaluateTree s `T.mapM` subtrees)

-- | 'evaluate' on a 'ProofTree' in parallel.
evaluateTreePar :: ProofData o => Strategy i o -> ProofTree i -> TctM (ProofTree o)
evaluateTreePar s t = spawnTree t >>= collect
  where
    spawnTree (Open p)  = Open <$> async (evaluate s p)
    spawnTree Fail = return Fail
    spawnTree (Success n certfn subtrees) = Success n certfn <$> (spawnTree `T.mapM` subtrees)

    collect (Open a)  = wait a
    collect Fail = return Fail
    collect (Success n certfn subtrees) = Success n certfn <$> (collect `T.mapM` subtrees)


--- * Strategy Processor ---------------------------------------------------------------------------------------------

-- |  Constructs a strategy declaration. For example: Assume that @st :: Int -> Maybe Int -> Strategy prob@.
--
-- > strategy "name" (nat, some nat) st
-- Similar to declare but specified to Strategies and with no description.
strategy ::
  ( ToHList as, HListOf as ~ args
  , f ~ Uncurry (ArgsType args :-> Ret (ArgsType args) f)
  , Ret (ArgsType args) f ~ Strategy prob prob)
  => String         -- ^ The name of the strategy.
  -> as             -- ^ The arguments as tuples: (), (OneTuple a1), (a1,a2) ...
  -> f              -- ^ The strategy.
  -> Declaration (args :-> Strategy prob prob)
strategy n = declare n []

