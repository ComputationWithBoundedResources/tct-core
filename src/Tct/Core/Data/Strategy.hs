-- | This module provides the 'Strategy' type.
module Tct.Core.Data.Strategy
  (
  Strategy (..)
  , Return (..)
  , returning
  , isProgressing
  , isContinuing
  , isAborting
  , isHalting

  -- * Strategy evaluation
  , evaluate
  , evaluateTree
  , evaluateTreePar
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


instance Show (Strategy i o) where
  show _ = "someStrategy"

instance PP.Pretty (Strategy i o) where
  pretty _ = PP.text "someStrategy"

returning :: (l -> a) -> (l -> a) -> a -> Return l -> a
returning f g a r = case r of
  Continue l -> f l
  Abort l    -> g l
  Halt _     -> a


isProgressing :: Return (ProofTree l) -> Bool
isProgressing (Continue pt) = progress pt
isProgressing _             = False

isContinuing :: Return l -> Bool
isContinuing (Continue _) = True
isContinuing _            = False

isAborting :: Return l -> Bool
isAborting (Abort _) = True
isAborting _         = False

isHalting :: Return l -> Bool
isHalting (Halt _) = True
isHalting _        = False

-- | Given two results returns the "better" one.
-- In order isProgressing > isContinuing > isAborting > isHalting, where the first argument is checked first.
better :: Return (ProofTree l) -> Return (ProofTree l) -> Return (ProofTree l)
better r1 r2
  | isProgressing r1 = r1
  | isProgressing r2 = r2
  | isContinuing r1  = r1
  | isContinuing r2  = r2
  | isAborting r1    = r1
  | isAborting r2    = r2
  | otherwise        = r1

-- | @'evaluate' s prob@ defines the application of @s@ to a problem.
-- See "Combinators" for a detailed description.
evaluate :: ProofData o => Strategy i o -> i -> TctM (Return (ProofTree o))
evaluate (Proc p) prob = do
  res <- solve p prob `catchError` errNode
  isContinuing res `seq` return res
  where errNode err = evaluate (Proc (ErroneousProc err p)) prob

evaluate (Trans s1 s2) prob = do
  r1 <- evaluate s1 prob
  case r1 of
    Continue pt -> evaluateTree s2 pt
    Abort pt    -> return (Halt $ ProofBox `fmap` pt)
    Halt pt     -> return (Halt pt)

evaluate (Trying True s) prob = do
  r1 <- evaluate s prob
  return $ case r1 of
    Continue pt -> Continue pt
    Abort pt    -> Continue pt
    Halt _      -> Continue (Open prob)

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
    _            -> return r1

evaluate (s1 `ThenPar` s2) prob = do
  r1 <- evaluate s1 prob
  case r1 of
    Continue pt1 -> evaluateTreePar s2 pt1
    _            -> return r1

evaluate (s1 `Alt` s2) prob = do
  r1 <- evaluate s1 prob
  if isProgressing r1
    then return r1
    else do
      r2 <- evaluate s2 prob
      return $ r1 `better` r2

evaluate (s1 `OrFaster` s2) prob =
  raceWith isProgressing better (evaluate s1 prob) (evaluate s2 prob)

evaluate (OrBetter cmp s1 s2) prob = do
  (r1, r2) <- concurrently (evaluate s1 prob) (evaluate s2 prob)
  return $ case (r1,r2) of
    (Continue pt1, Continue pt2)
      | progress pt1 && progress pt2 -> Continue $ maxBy cmp pt1 pt2
    _ -> r1 `better` r2
  where maxBy cm pt1 pt2 = if cm pt2 pt1 == GT then pt2 else pt1

liftNoProgress :: Processor p => ProofNode p -> Return (ProofTree l) -> Return (ProofTree l)
liftNoProgress n (Continue pt) = Continue (NoProgress n pt)
liftNoProgress n (Abort pt)    = Abort (NoProgress n pt)
liftNoProgress n (Halt pt)     = Halt (NoProgress n pt)

liftProgress :: (Processor p, ProofData l) => ProofNode p -> CertificateFn p -> Forking p (Return (ProofTree l)) -> Return (ProofTree l)
liftProgress n certfn rs
  | F.any isHalting rs  = tree2
  | F.any isAborting rs = Abort tree1
  | otherwise           = Continue tree1
  where
    tree1 = Progress n certfn (fromReturn `fmap` rs)
    tree2 = Halt $ Progress n certfn (k `fmap` rs)
      where
        k (Halt pt) = pt
        k r         = ProofBox `fmap` fromReturn r

-- | 'evaluate' on a 'ProofTree'.
evaluateTree :: ProofData o => Strategy i o -> ProofTree i -> TctM (Return (ProofTree o))
evaluateTree s (Open p)                     = evaluate s p
evaluateTree s (NoProgress n subtree)       = liftNoProgress n `fmap` evaluateTree s subtree
evaluateTree s (Progress n certfn subtrees) = liftProgress n certfn `fmap` (evaluateTree s `T.mapM` subtrees)

-- | 'evaluate' on a 'ProofTree' in parallel.
evaluateTreePar :: ProofData o => Strategy i o -> ProofTree i -> TctM (Return (ProofTree o))
evaluateTreePar s t = spawnTree t >>= collect
  where
    spawnTree (Open p)                     = Open `fmap` async (evaluate s p)
    spawnTree (NoProgress n subtree)       = NoProgress n `fmap` spawnTree subtree
    spawnTree (Progress n certfn subtrees) = Progress n certfn `fmap` (spawnTree `T.mapM` subtrees)

    collect (Open a)                     = wait a
    collect (NoProgress n subtree)       = liftNoProgress n `fmap` collect subtree
    collect (Progress n certfn subtrees) = liftProgress n certfn `fmap` (collect `T.mapM` subtrees)


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

