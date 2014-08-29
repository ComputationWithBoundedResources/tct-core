-- | This module provides the 'Strategy' type.
module Tct.Core.Strategy
  (
  Strategy (..)
  , Return (..)
  , evaluate

  -- * Customised Strategy
  , CustomStrategy (..)
  , strategy
  ) where


import           Control.Monad       (liftM)
import           Control.Monad.Error (catchError)
import           Data.Foldable       as F
import           Data.Monoid         (mempty)
import           Data.Traversable    as T
import qualified Options.Applicative as O

import           Tct.Common.Error    (TctError (..))
import           Tct.Common.Parser   (tokenise)
import qualified Tct.Common.Pretty   as PP
import           Tct.Core.Processor
import           Tct.Core.ProofTree
import           Tct.Core.TctM
--import qualified Tct.Xml as Xml

-- | A 'Strategy' composes instances of 'Processor' and specifies in which order they are applied.
-- For a detailed description of the control flow constructs see "Combinators".
-- 'Strategy' is an instance of 'Processor', hence they can be used in processor combinators. For example,
--
-- > timoutIn 20 (s1 >>> s2)
data Strategy prob where
  Proc       :: SomeProcessor prob -> Strategy prob
  Trying     :: Bool -> Strategy prob -> Strategy prob
  Then       :: Strategy prob -> Strategy prob -> Strategy prob
  ThenPar    :: Strategy prob -> Strategy prob -> Strategy prob
  Alt        :: Strategy prob -> Strategy prob -> Strategy prob
  OrFaster   :: Strategy prob -> Strategy prob -> Strategy prob
  OrBetter   :: (ProofTree prob -> ProofTree prob -> Ordering) -> Strategy prob -> Strategy prob -> Strategy prob
  WithStatus :: (TctStatus prob -> Strategy prob) -> Strategy prob

instance Show (Strategy prob) where show _ = "ShowStrategy"

-- | 'Return' specifies if the evaluation of a strategy is aborted or continued.
-- See "Combinators" fndor a detailed description.
data Return l
  = Continue { fromReturn :: l }
  | Abort    { fromReturn :: l }
  deriving (Show, Functor)

isContinuing :: Return (ProofTree prob) -> Bool
isContinuing (Continue _) = True
isContinuing _            = False

isProgressing :: Return (ProofTree prob) -> Bool
isProgressing (Continue pt) = progress pt
isProgressing _             = False

-- | @'evaluate' s prob@ defines the application of @s@ to a problem.
-- See "Combinators" for a detailed description.
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


-- TODO :
-- test if threads leak in combination of timeout
-- if so; try using withAsync, concurrently, or at least invoke them within timeout
evaluateTreePar :: Strategy prob -> ProofTree prob -> TctM (Return (ProofTree prob))
evaluateTreePar s t = spawnTree t >>= collect
  where
    spawnTree (Open p)                     = Open `fmap` async (evaluate s p)
    spawnTree (NoProgress n subtree)       = NoProgress n `fmap` spawnTree subtree
    spawnTree (Progress n certfn subtrees) = Progress n certfn `fmap` (spawnTree `T.mapM` subtrees)

    collect (Open a)                     = wait a
    collect (NoProgress n subtree)       = liftNoProgress n `fmap` collect subtree
    collect (Progress n certfn subtrees) = liftProgress n certfn `fmap` (collect `T.mapM` subtrees)



-- Error Processor -----------------------------------------------------------
data ErroneousProof p = ErroneousProof IOError p deriving Show

--instance Processor p => Xml.Xml (ErroneousProof p) where
  --toXml (ErroneousProof err p) =
    --Xml.elt "error" [] [ Xml.elt "processor" [] [Xml.text (name p)]
                       --, Xml.elt "message" [] [Xml.text (show err)] ]

instance Processor p => PP.Pretty (ErroneousProof p) where
  pretty (ErroneousProof err p) =
    PP.text "Processor" PP.<+> PP.squotes (PP.text (name p)) PP.<+> PP.text "signalled the following error:"
    PP.<$$> PP.indent 2 (PP.paragraph (show err))

data ErroneousProcessor p = ErroneousProc IOError p deriving Show

instance Processor p => Processor (ErroneousProcessor p) where
  type ProofObject (ErroneousProcessor p) = ErroneousProof p
  type Problem (ErroneousProcessor p)     = Problem p
  name (ErroneousProc err p)              = name p ++ "[error: " ++ show err ++ "]"
  solve (ErroneousProc err p) _           = return (Fail (ErroneousProof err p))

instance Processor p => ParsableProcessor (ErroneousProcessor p) where


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


-- Customised Strategy -----------------------------------------------------------------------------------------------

-- | @'strategy' name argumentParser strategy defaultArguments@ constructs a 'CostumStrategy'.
strategy :: String -> O.ParserInfo args -> (args -> Strategy prob) -> args -> CustomStrategy args prob
strategy nme pargs st stargs = CustomStrategy nme stargs pargs st

-- | 'CustomStrategy' implements 'Processor' and 'ParsableProcessor',  and is used to generate a parser for strategies.
-- The recommended way for strategies without arguments is 'Combinators.named'.
-- For example:
--
-- @
-- direct i = timeoutIn $ strat1 >>> strat2
-- strat1 = strategy "direct" pargs direct (-1)
--   where
--     cargs = option $ eopt
--       `withArgLong` "timeout"
--       `withHelpDoc` PP.paragraph "abort after nSec seconds"
--       `withMetavar` "nSec"
--     pargs = mkArgParser cargs (PP.paragraph  "do strat1 and strat2 with timeout")
-- @
-- The string @"direct --timeout 10"@ is parsed successfully.
--
-- If a custom strategy is used within another strategy the default arguments 'args_' are used.
data CustomStrategy args prob = CustomStrategy
  { name_     :: String
  , args_     :: args
  , pargs_    :: O.ParserInfo args
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

-- TODO: refactor with Processor
instance ProofData prob => ParsableProcessor (CustomStrategy arg prob) where
  args p _ = SomeProc `fmap` const p `fmap` pargs_ p
  parseProcessor p _ ss = do
    (t,ts) <- tokenise ss
    if name p == t
      then case O.execParserPure (O.prefs mempty) (pargs_ p) ts of
        O.Success a   -> Right $ SomeProc $ p {args_ = a}
        O.Failure err -> Left $ TctParseError $ "optParser error (" ++ show err ++ "," ++ show ss ++ ")"
        _             -> Left $ TctParseError $ "optParser completion error (" ++ show ss ++ ")"
      else Left $ TctParseError $ name p ++ ss


-- we use strategy evaluation to make SomeProcessor an instance of Processor
-- as we can not extract the Forking type of (SomeProcessor p) a direct instance did not work out
data SomeProofObject               where SomeProofObj :: (ProofData obj) => obj -> SomeProofObject
instance PP.Pretty SomeProofObject where pretty (SomeProofObj obj) = PP.pretty obj
instance Show SomeProofObject      where show (SomeProofObj obj)   = show obj

instance ProofData prob => Processor (SomeProcessor prob) where
  type ProofObject (SomeProcessor prob) = StrategyProof prob
  type Problem (SomeProcessor prob) = prob
  type Forking (SomeProcessor prob) = ProofTree
  name (SomeProc p) = name p
  solve p prob = do
    pt <- fromReturn `liftM` evaluate (Proc p) prob
    return $ if progress pt
      then Success pt (StrategyProof pt) collectCertificate
      else Fail (StrategyProof pt)

instance ProofData prob => ParsableProcessor (SomeProcessor prob) where
  parseProcessor (SomeProc p) = parseProcessor p

