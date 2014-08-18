module Tct.Core.Strategy
  (
    Strategy (..)
  , evaluate
  , Return
  , fromReturn

  , some
  , try
  , force 
  , (>>>), (>||), (<|>)

  , CustomStrategy (..)
  , strategy
  ) where


import           Control.Monad (liftM)
import           Control.Monad.Error (catchError)
import           Data.Monoid (mempty)
import           Data.Foldable as F
import           Data.Traversable as T
import qualified Options.Applicative as O 

import           Tct.Core.TctM
import           Tct.Core.Processor
import           Tct.Core.ProofTree
import qualified Tct.Pretty as PP
--import qualified Tct.Xml as Xml

data Strategy prob where
  Proc       :: SomeProcessor prob -> Strategy prob
  Trying     :: Bool -> Strategy prob -> Strategy prob
  (:>>>:)    :: Strategy prob -> Strategy prob -> Strategy prob
  (:>||>:)   :: Strategy prob -> Strategy prob -> Strategy prob
  (:<>:)     :: Strategy prob -> Strategy prob -> Strategy prob
  WithStatus :: (TctStatus prob -> Strategy prob) -> Strategy prob

instance Show (Strategy prob) where show _ = "ShowStrategy"


some :: (Processor p, ParsableProcessor p)  => p ->  Strategy (Problem p)
some = Proc . SomeProc

try :: Strategy prob -> Strategy prob
try s@(Trying _ _) = Trying True s
try (WithStatus f) = WithStatus (try . f)
try s              = Trying True s

force :: Strategy prob -> Strategy prob
force s@(Trying _ _) = Trying False s
force (WithStatus f) = WithStatus (force . f)
force s              = Trying False s

(>>>), (>||), (<|>) :: Strategy prob -> Strategy prob -> Strategy prob
(>>>) = (:>>>:)
(>||) = (:>||>:)
(<|>) = (:<>:)


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
instance Show (StrategyProof prob) where  show (StrategyProof _) = "StrategyProof"
instance ProofData prob => PP.Pretty (StrategyProof prob) where  pretty (StrategyProof pt) = PP.pretty pt

instance ProofData prob => Processor (Strategy prob) where
  type ProofObject (Strategy prob) = StrategyProof prob
  type Forking (Strategy prob)     = ProofTree
  type Problem (Strategy prob)     = prob
  name = const "Strategy Evaluation"
  solve s prob = do
    pt <- fromReturn `liftM` evaluate s prob
    return $ if progress pt
      then Success pt (StrategyProof pt) collectCertificate
      else Fail (StrategyProof pt)


-- custom strategies
strategy :: String -> O.Parser args -> (args -> Strategy prob) -> args -> CustomStrategy args prob
strategy nme pargs st stargs = CustomStrategy nme stargs pargs st

data CustomStrategy args prob = CustomStrategy 
  { name_ :: String
  , args_ :: args
  , pargs_ :: O.Parser args
  , strategy_ :: args -> Strategy prob }

instance Show (CustomStrategy args prob) where show = show . name_

instance ProofData prob => Processor (CustomStrategy arg prob) where
  type ProofObject (CustomStrategy arg prob) = StrategyProof prob
  type Forking (CustomStrategy arg prob)     = ProofTree
  type Problem (CustomStrategy arg prob)     = prob
  name                                       = name_
  solve st prob = do
    pt <- fromReturn `liftM` evaluate (strategy_ st (args_ st)) prob
    return $ if progress pt
      then Success pt (StrategyProof pt) collectCertificate
      else Fail (StrategyProof pt)

instance ProofData prob => ParsableProcessor (CustomStrategy arg prob) where
  -- args p _ = SomeProc `O.liftA` strategy_ p `O.liftA` pargs_ p
  readProcessor p _ ss
    | name p == t =
      case O.execParserPure (O.prefs mempty) (O.info (pargs_ p) O.briefDesc) ts of
        O.Success a   -> Right $ SomeProc $ p {args_ = a}
        O.Failure err -> Left $ "optParser error (" ++ show err ++ "," ++ show ss ++ ")"
        _             -> Left $ "optParser completion error (" ++ show ss ++ ")"
    | otherwise = Left $ name p ++ ss
    where (t:ts) = words ss

-- make Processor (SomeProcessor prob) mainly for parsing;
data SomeProofObject               where SomeProofObj :: (ProofData obj) => obj -> SomeProofObject
instance PP.Pretty SomeProofObject where pretty (SomeProofObj obj) = PP.pretty obj
instance Show SomeProofObject      where show (SomeProofObj obj)   = show obj

instance ProofData prob => Processor (SomeProcessor prob) where
  type ProofObject (SomeProcessor prob) = StrategyProof prob
  type Problem (SomeProcessor prob) = prob
  type Forking (SomeProcessor prob) = ProofTree
  name (SomeProc p) = name p
  solve p prob = do
    pt <- fromReturn `liftM` evaluate (Proc $ p) prob
    return $ if progress pt
      then Success pt (StrategyProof pt) certfn
      else Fail (StrategyProof pt)
    where
      certfn (Open c)                      = c
      certfn (NoProgress _ subtree)        = certfn subtree
      certfn (Progress _ certfn' subtrees) = certfn' (certfn `fmap` subtrees)

instance ProofData prob => ParsableProcessor (SomeProcessor prob) where
  readProcessor (SomeProc p) = readProcessor p

