-- | This module provides the 'Strategy' type.
module Tct.Core.Data.Strategy
  (
  Strategy (..)
  , Return (..)
  , returning
  , isContinuing
  , isProgressing
  , evaluate
  , liftProgress
  , liftNoProgress
  -- * Declaration
  , StrategyDeclaration (..)
  , strategy
  ) where


import           Control.Monad.Error     (catchError)
import           Data.Foldable           as F
import           Data.Traversable        as T

import qualified Tct.Core.Common.Pretty  as PP
import           Tct.Core.Data.Processor
import           Tct.Core.Data.ProofTree
import           Tct.Core.Data.TctM
import           Tct.Core.Data.Types
--import qualified Tct.Xml as Xml


instance Show (Strategy i o) where
  show _ = "someStrategy" -- TODO

instance PP.Pretty (Strategy i o) where
  pretty _ = PP.text "someStrategy" -- TODO

returning :: (l -> a) -> (l -> a) -> a -> Return l -> a
returning f g a r = case r of
  Continue l -> f l
  Abort l    -> g l
  Flop       -> a

isContinuing :: Return (ProofTree prob) -> Bool
isContinuing (Continue _) = True
isContinuing _            = False

isProgressing :: Return (ProofTree prob) -> Bool
isProgressing (Continue pt) = progress pt
isProgressing _             = False

-- | @'evaluate' s prob@ defines the application of @s@ to a problem.
-- See "Combinators" for a detailed description.
evaluate :: Strategy i o -> i -> TctM (Return (ProofTree o))
evaluate (Proc p) prob = solve p prob `catchError` errNode
  where errNode err = evaluate (Proc (ErroneousProc err p)) prob

evaluate (Trans s1 s2) prob = do
  r1 <- evaluate s1 prob
  case r1 of
    Continue pt -> evaluateTree s2 pt
    Abort _     -> return Flop
    Flop        -> return Flop

evaluate (Trying True s) prob = do
  r1 <- evaluate s prob
  return $ case r1 of
    Continue pt -> Continue pt
    Abort pt    -> Continue pt
    Flop        -> Continue (Open prob)

evaluate (Trying False s) prob = do
  r1 <- evaluate s prob
  return $ case r1 of
    Continue pt
      | progress pt -> Continue pt
      | otherwise   -> Abort pt
    pt -> pt

evaluate (WithStatus f) prob = do
  st <- askStatus prob
  evaluate (f st) prob

evaluate (s1 `Then` s2) prob = do
  r1 <- evaluate s1 prob
  case r1 of
    Continue pt1 -> evaluateTree s2 pt1
    Abort pt1    -> return (Abort pt1)
    Flop         -> return Flop

evaluate (s1 `ThenPar` s2) prob = do
  r1 <- evaluate s1 prob
  case r1 of
    Continue pt1 -> evaluateTreePar s2 pt1
    Abort pt1    -> return (Abort pt1)
    Flop         -> return Flop

evaluate (s1 `Alt` s2) prob = do
  r1 <- evaluate s1 prob
  case r1 of
    Continue pt1
      | progress pt1 -> return (Continue pt1)
    _ -> evaluate s2 prob

evaluate (s1 `OrFaster` s2) prob =
  raceWith isProgressing isContinuing (evaluate s1 prob) (evaluate s2 prob)

evaluate (OrBetter cmp s1 s2) prob = do
  (r1, r2) <- concurrently (evaluate s1 prob) (evaluate s2 prob)
  return $ case (r1,r2) of
    (Continue pt1, Continue pt2)
      | progress pt1 && progress pt2 -> Continue $ maxBy cmp pt1 pt2
      | progress pt1                 -> r1
      | otherwise                    -> r2
    _                                -> r2
  where maxBy cm pt1 pt2 = if cm pt2 pt1 == GT then pt2 else pt1

liftNoProgress :: Processor p => ProofNode p -> Return (ProofTree l) -> Return (ProofTree l)
liftNoProgress n (Continue pt) = Continue (NoProgress n pt)
liftNoProgress n (Abort pt)    = Abort (NoProgress n pt)
liftNoProgress _ Flop          = Flop

liftProgress :: Processor p => ProofNode p -> CertificateFn p -> Forking p (Return (ProofTree l)) -> Return (ProofTree l)
liftProgress n certfn rs
  | F.any isFlop rs  = Flop
  | F.any isAbort rs = Abort tree
  | otherwise        = Continue tree
  where
    tree = Progress n certfn (fromReturn `fmap` rs)
    isFlop Flop       = True
    isFlop _          = False
    isAbort (Abort _) = True
    isAbort _         = False

evaluateTree :: Strategy i o -> ProofTree i -> TctM (Return (ProofTree o))
evaluateTree s (Open p)                     = evaluate s p
evaluateTree s (NoProgress n subtree)       = liftNoProgress n `fmap` evaluateTree s subtree
evaluateTree s (Progress n certfn subtrees) = liftProgress n certfn `fmap` (evaluateTree s `T.mapM` subtrees)

evaluateTreePar :: Strategy i o -> ProofTree i -> TctM (Return (ProofTree o))
evaluateTreePar s t = spawnTree t >>= collect
  where
    spawnTree (Open p)                     = Open `fmap` async (evaluate s p)
    spawnTree (NoProgress n subtree)       = NoProgress n `fmap` spawnTree subtree
    spawnTree (Progress n certfn subtrees) = Progress n certfn `fmap` (spawnTree `T.mapM` subtrees)

    collect (Open a)                     = wait a
    collect (NoProgress n subtree)       = liftNoProgress n `fmap` collect subtree
    collect (Progress n certfn subtrees) = liftProgress n certfn `fmap` (collect `T.mapM` subtrees)


-- Strategy Processor -----------------------------------------------------------------------------------------------

-- |  Constructs a strategy declaration. For example: Assume that @st :: Int -> Maybe Int -> Strategy prob@.
--
-- > strategy "name" (nat, some nat) st
strategy :: 
  ( ToHList as, HListOf as ~ args
  , f ~ Uncurry (ArgsType args :-> Ret (ArgsType args) f)
  , Ret (ArgsType args) f ~ Strategy prob prob)
  => String -- ^ The name of the strategy.
  -> as  -- ^ The arguments as tuples: (), (OneTuple a1), (a1,a2) ...
  -> f  -- ^ The strategy.
  -> Declaration (args :-> Strategy prob prob)
strategy n = declare n [] 

