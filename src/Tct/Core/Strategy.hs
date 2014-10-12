-- | This module provides the 'Strategy' type.
module Tct.Core.Strategy
  (
  Strategy (..)
  , Return (..)
  , returning
  , evaluate
  -- * Declaration
  , strategy
  -- * Answer
  , Answer
  , answer
  ) where


import           Control.Monad.Error (catchError)
import           Data.Foldable       as F
import           Data.Traversable    as T

import qualified Tct.Common.Pretty   as PP
import           Tct.Core.Types
import           Tct.Core.Processor 
import           Tct.Core.ProofTree 
import           Tct.Core.TctM
--import qualified Tct.Xml as Xml


instance Show (Strategy prob) where
  show _ = "someStrategy" -- TODO

instance PP.Pretty (Strategy prob) where
  pretty _ = PP.text "someStrategy" -- TODO

-- | @'returning f g r' returns @f (fromReturn r)@ if @r@ is continuing, otherwise @g (fromReturn r)@.
returning :: (l -> a) -> (l -> a) -> Return l-> a
returning f g r = case r of
  Continue l -> f l
  Abort l    -> g l

isContinuing :: Return (ProofTree prob) -> Bool
isContinuing (Continue _) = True
isContinuing _            = False

isProgressing :: Return (ProofTree prob) -> Bool
isProgressing (Continue pt) = progress pt
isProgressing _             = False

-- | @'evaluate' s prob@ defines the application of @s@ to a problem.
-- See "Combinators" for a detailed description.
evaluate :: Strategy prob -> prob -> TctM (Return (ProofTree prob))
evaluate (Proc p) prob = solve p prob `catchError` errNode
  where errNode err = evaluate (Proc (ErroneousProc err p)) prob

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

evaluate (s1 `Then` s2) prob = do
  r1 <- evaluate s1 prob
  case r1 of
    Abort pt1    -> return (Abort pt1)
    Continue pt1 -> evaluateTree s2 pt1

evaluate (s1 `ThenPar` s2) prob = do
  r1 <- evaluate s1 prob
  case r1 of
    Abort pt1 -> return (Abort pt1)
    Continue pt1 -> evaluateTreePar s2 pt1

evaluate (s1 `Alt` s2) prob = do
  r1 <- evaluate s1 prob
  case r1 of
    Continue pt1
      | progress pt1 -> return (Continue pt1)
      | otherwise    -> do
          r2 <- evaluate s2 prob
          case r2 of
            Abort _      -> return (Continue pt1)
            Continue pt2 -> return (Continue pt2)
    Abort _ -> evaluate s2 prob

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

-- TODO: test if leaks; then use concurrently
evaluateTreePar :: Strategy prob -> ProofTree prob -> TctM (Return (ProofTree prob))
evaluateTreePar s t = spawnTree t >>= collect
  where
    spawnTree (Open p)                     = Open `fmap` async (evaluate s p)
    spawnTree (NoProgress n subtree)       = NoProgress n `fmap` spawnTree subtree
    spawnTree (Progress n certfn subtrees) = Progress n certfn `fmap` (spawnTree `T.mapM` subtrees)

    collect (Open a)                     = wait a
    collect (NoProgress n subtree)       = liftNoProgress n `fmap` collect subtree
    collect (Progress n certfn subtrees) = liftProgress n certfn `fmap` (collect `T.mapM` subtrees)


-- Strategy Processor -----------------------------------------------------------------------------------------------

strategy :: 
  ( ToHList as, HListOf as ~ args
  , f ~ Uncurry (ArgsType args :-> Ret (ArgsType args) f)
  , Ret (ArgsType args) f ~ Strategy prob )
  => String -> as -> f -> Declaration (args :-> Strategy prob)
strategy n as f = Decl n [] f (toHList as)



-- | prop> anwer = Answer
answer :: ProofData a => a -> Answer
answer = Answer

