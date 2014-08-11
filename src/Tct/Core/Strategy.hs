{-# LANGUAGE DeriveDataTypeable #-}
module Tct.Core.Strategy
where

import           Control.Monad (liftM)
import           Control.Monad.Error (catchError)
import           Data.Foldable as F
import           Data.Traversable as T
import           Data.Typeable

import           Tct.Core.TctM
import           Tct.Core.Processor
import           Tct.Core.ProofTree
import qualified Tct.Pretty as PP
import qualified Tct.Xml as Xml


data Strategy prob where
  Proc       :: SomeProcessor prob -> Strategy prob
  Trying     :: Bool -> Strategy prob -> Strategy prob
  (:>>>:)    :: Strategy prob -> Strategy prob -> Strategy prob
  (:>||>:)   :: Strategy prob -> Strategy prob -> Strategy prob
  (:<>:)     :: Strategy prob -> Strategy prob -> Strategy prob
  WithStatus :: (TctStatus prob -> Strategy prob) -> Strategy prob
  deriving Typeable

instance Show (Strategy prob) where show _ = "ShowStrategy"

try :: Strategy prob -> Strategy prob
try s@(Trying _ _) = Trying True s
try (WithStatus f) = WithStatus (try . f)
try s              = Trying True s

force :: Strategy prob -> Strategy prob
force s@(Trying _ _) = Trying False s
force (WithStatus f) = WithStatus (force . f)
force s              = Trying False s


data Return l = Continue l | Abort l deriving Show

fromReturn :: Return l -> l
fromReturn (Continue l) = l
fromReturn (Abort l)    = l

evaluate :: Strategy prob -> prob -> TctM (Return (ProofTree prob))
evaluate (Proc (SomeProc p)) prob = (f `fmap` solve p prob) `catchError` errNode
  where
    f res@(Fail {})    = Abort (resultToTree prob p res)
    f res@(Success {}) = Continue (resultToTree prob p res)
    errNode err = evaluate (Proc (SomeProc $ ErroneousProc err p)) prob

evaluate (Trying True s) prob = f `fmap` evaluate s prob
  where
    f (Abort pt) = Continue pt
    f pt         = pt

evaluate (Trying False s) prob = f `fmap` evaluate s prob
    where
      f (Continue pt)
          | progress pt = Continue pt
          | otherwise = Abort pt
      f pt = pt

evaluate (WithStatus f) prob = do
  st <- askStatus prob
  evaluate (f st) prob

evaluate (s1 :>>>: s2) prob = do
  r1 <- evaluate s1 prob
  case r1 of
    Abort pt1 -> return (Abort pt1)
    Continue pt1 -> evaluateTree s2 pt1

evaluate (s1 :>||>: s2) prob = do
  r1 <- evaluate s1 prob
  case r1 of
    Abort pt1 -> return (Abort pt1)
    Continue pt1 -> evaluateTreePar s2 pt1

evaluate (s1 :<>: s2) prob = do
  r1 <- evaluate s1 prob
  case r1 of
    Continue pt1
        | progress pt1 -> return (Continue pt1)
        | otherwise -> do
            r2 <- evaluate s2 prob
            case r2 of
              Abort _ -> return (Continue pt1)
              Continue pt2 -> return (Continue pt2)
    Abort pt1 -> return (Abort pt1)


liftNoProgress :: Processor p => ProofNode p -> Return (ProofTree l) -> Return (ProofTree l)
liftNoProgress n (Continue pt) = Continue (NoProgress n pt)
liftNoProgress n (Abort pt) = Abort (NoProgress n pt)

liftProgress :: Processor p => ProofNode p -> CertificateFn p -> Forking p (Return (ProofTree l)) -> Return (ProofTree l)
liftProgress n certfn rs
  | F.any isAbort (toList rs) = Abort tree
  | otherwise = Continue tree
  where
    tree = Progress n certfn (fromReturn `fmap` rs)
    isAbort (Abort _) = True
    isAbort _         = False

evaluateTree :: Strategy prob -> ProofTree prob -> TctM (Return (ProofTree prob))
evaluateTree s (Open p)                     = evaluate s p
evaluateTree s (NoProgress n subtree)       = liftNoProgress n `fmap` evaluateTree s subtree
evaluateTree s (Progress n certfn subtrees) = liftProgress n certfn `fmap` (evaluateTree s `T.mapM` subtrees)

evaluateTreePar :: Strategy prob -> ProofTree prob -> TctM (Return (ProofTree prob))
evaluateTreePar s t = spawnTree t >>= collect
    where
    --spawnTree :: ProofTree prob -> TctM (ProofTree (Async (Return (ProofTree prob))))
    spawnTree (Open p)                     = Open `fmap` async (evaluate s p)
    spawnTree (NoProgress n subtree)       = NoProgress n `fmap` spawnTree subtree
    spawnTree (Progress n certfn subtrees) = Progress n certfn `fmap` (spawnTree `T.mapM` subtrees)
    --collect :: ProofTree (Async.Async (Return (ProofTree prob))) -> TctM (Return (ProofTree prob))
    collect (Open a)                     = wait a
    collect (NoProgress n subtree)       = liftNoProgress n `fmap` collect subtree
    collect (Progress n certfn subtrees) = liftProgress n certfn `fmap` (collect `T.mapM` subtrees)


-- Strategy Processor --------------------------------------------------------
-- lift Strategies to Processor
data StrategyProof prob = StrategyProof (ProofTree prob)
instance Show (StrategyProof prob)      where show   = undefined
instance Xml.Xml (StrategyProof prob)   where toXml  = undefined
instance ProofData prob => PP.Pretty (StrategyProof prob) where 
  pretty (StrategyProof pt) = PP.pretty pt

data StrategyProcessor prob = StrategyProc (Strategy prob)

instance ProofData prob => Show (StrategyProcessor prob) where show = undefined

--instance ProofData prob => Processor (StrategyProcessor prob) where
  --type ProofObject (StrategyProcessor prob) = StrategyProof prob
  --type Forking (StrategyProcessor prob) = ProofTree
  --type Problem (StrategyProcessor prob) = prob
  --name = const "Strategy Evaluation"
  --solve (StrategyProc s) prob = do
    --pt <- fromReturn `liftM` evaluate s prob
    --return $ if progress pt
      --then Success pt (StrategyProof pt) certfn
      --else Fail (StrategyProof pt)
    --where
      ---- collect the results
      --certfn (Open c)                      = c
      --certfn (NoProgress _ subtree)        = certfn subtree
      --certfn (Progress _ certfn' subtrees) = certfn' (certfn `fmap` subtrees)

instance (Typeable prob, ProofData prob) => Processor (Strategy prob) where
  type ProofObject (Strategy prob) = StrategyProof prob
  type Forking (Strategy prob) = ProofTree
  type Problem (Strategy prob) = prob
  name = const "Strategy Evaluation"
  solve s prob = do
    pt <- fromReturn `liftM` evaluate s prob
    return $ if progress pt
      then Success pt (StrategyProof pt) certfn
      else Fail (StrategyProof pt)
    where
      -- collect the results
      certfn (Open c)                      = c
      certfn (NoProgress _ subtree)        = certfn subtree
      certfn (Progress _ certfn' subtrees) = certfn' (certfn `fmap` subtrees)
