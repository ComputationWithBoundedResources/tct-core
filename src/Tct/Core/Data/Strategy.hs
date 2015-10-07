-- | This module provides the 'Strategy' type.
module Tct.Core.Data.Strategy
  (
  Strategy (..)
  -- * Strategy evaluation
  , evaluate
  -- , evaluatePar
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

-- pickNonFail :: ProofTree o -> ProofTree o -> ProofTree o
-- pt1@Success{} `pickNonFail` _             = pt1
-- _             `pickNonFail` pt2@Success{} = pt2
-- pt1@Open{}    `pickNonFail` _             = pt1
-- _             `pickNonFail` pt2@Open{}    = pt2
-- pt1           `pickNonFail` _             = pt1

-- | @'evaluate1' s prob@ defines the application of @s@ to a problem.
-- See "Combinators" for a detailed description.
evaluate1 :: ProofData o => Strategy i o -> i -> TctM (ProofTree o)
evaluate1 (Apply p) prob = apply p prob
evaluate1 (Sequence s1 s2) prob = do
  pt <- evaluate1 s1 prob
  if failure pt
   then return Fail
   else evaluate s2 pt
evaluate1 (Alternative s1 s2) prob = do
  pt <- evaluate1 s1 prob
  if failure pt
   then evaluate1 s2 prob
   else return pt
evaluate1 (Try s) prob = do
  pt <- evaluate1 s prob
  if failure pt
   then return (Open prob)
   else return pt
evaluate1 (Force s) prob = do
  pt <- evaluate1 s prob
  case pt of
    Open{} -> return Fail
    _ -> return pt
evaluate1 (Par s) prob = evaluate1 s prob
evaluate1 (Race s1 s2) prob =
  raceWith (not . failure) const (evaluate1 s1 prob) (evaluate1 s2 prob)
evaluate1 (Better cmp s1 s2) prob =
  uncurry pick <$> concurrently (evaluate1 s1 prob) (evaluate1 s2 prob) where
    pick r1 r2 | cmp r1 r2 == GT = r2
               | otherwise = r1
evaluate1 (WithStatus f) prob = do
  st <- askStatus prob
  evaluate1 (f st) prob
evaluate1 (WithState f s) prob =
  setState f (evaluate1 s prob)



-- | 'evaluate' on a 'ProofTree'.
evaluate :: ProofData o => Strategy i o -> ProofTree i -> TctM (ProofTree o)
evaluate (Par s) = evaluatePar s
evaluate s = evaluateSeq s
-- evaluate s (Open p)  = evaluate s p
-- evaluate s Fail      = return Fail
-- evaluate s (Success pn certfn subtrees) =
--   Success pn certfn <$> (evaluateTree s `T.mapM` subtrees)

-- | 'evaluate' on a 'ProofTree' in parallel.
evaluateSeq :: ProofData o => Strategy i o -> ProofTree i -> TctM (ProofTree o)
evaluateSeq s (Open i) = evaluate1 s i
evaluateSeq _ Fail = return Fail
evaluateSeq s (Success pn certfn subtrees) =
-- MA:TODO: implement early abort  
  Success pn certfn <$> (evaluateSeq s `T.mapM` subtrees)
            
-- | 'evaluate' on a 'ProofTree' in parallel.
evaluatePar :: ProofData o => Strategy i o -> ProofTree i -> TctM (ProofTree o)
evaluatePar s t = spawnTree t >>= collect
  where
    spawnTree (Open p)  = Open <$> async (evaluate1 s p)
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

