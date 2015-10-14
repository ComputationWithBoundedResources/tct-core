{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators             #-}
-- | This module provides a basic interactive functionality via \ghci\.
--
-- __WARNING:__ all operations that are directly applied to a problem are __unsafe__. More precisely, the application
-- of a function to a problem is not type-safe: Given problems of type @i@. We can apply a strategy of type @Strategy
-- o o@ (where @o@ different from @i@). This can lead to undefined behaviour.
--
-- \At the momement user configurations and user options are ignored.\
module Tct.Core.Interactive
  (
  -- * Load and Modify Problems
  load
  , load'
  , modifyProblems
  , onProblems
  , onTree

  -- * Select (Sub-)Problems
  -- | We can select sub-problems individually and use 'applySelected' to apply strategies only on selected
  -- sub-problems. Use 'printState' to  get a list of open sub-problems.
  , select
  , selectAll

  -- * Apply Strategies
  , apply
  , applySelected

  -- * Undo History
  , undo
  , reset

  -- * Print State
  -- | For simplicity the output is fixed; And unrelated to user-defined options and configurations. Use 'onProblems',
  -- 'onTree' to perform specific IO actions.
  , printState
  , printProof
  , printDetailedProof
  ) where


import           Control.Applicative        ((<$>))
import           Control.Monad
import qualified Control.Monad.State.Strict as S
import           Data.Either                (rights)
import qualified Data.Foldable              as F
import           Data.IORef
import qualified Data.Traversable           as F
import           System.IO.Unsafe

import           Tct.Core.Common.Error
import qualified Tct.Core.Common.Pretty     as PP
import qualified Tct.Core.Common.Xml        as Xml
import           Tct.Core.Data              hiding (apply, proof)
import           Tct.Core.Main


--- * selection ------------------------------------------------------------------------------------------------------

type Selected l = Either l l

instance PP.Pretty l => PP.Pretty (Selected l) where
  pretty (Left _)  = PP.empty
  pretty (Right l) = PP.pretty l

instance Xml.Xml l => Xml.Xml (Selected l) where
  toXml = const Xml.empty

selectLeafs :: ProofData l => [Int] -> ProofTree (Selected l) -> ProofTree (Selected l)
selectLeafs ns pt = S.evalState (F.mapM k pt) 0
  where
    k p = S.modify succ >> S.get >>= \i -> return (either (f i) (f i) p)
    f i = if i `elem` ns then Right else Left

selectAllLeafs :: ProofTree (Selected l) -> ProofTree (Selected l)
selectAllLeafs = fmap (either Right Right)

removeSelection :: ProofTree (Selected l) -> ProofTree l
removeSelection = fmap (either id id)

-- MS: Strategies with (possible) different input output type have to be applied to all problems.
evaluateAll :: ProofData o => Strategy i o -> ProofTree (Selected i) -> TctM (ProofTree (Selected o))
evaluateAll s (Open (Left p))             = fmap Left `fmap` evaluate s (Open p)
evaluateAll s (Open (Right p))            = fmap Right `fmap` evaluate s (Open p)
evaluateAll _ (Failure r)                 = return (Failure r)
evaluateAll s (Success n certfn subtrees) = Success n certfn <$> (evaluateAll s `F.mapM` subtrees)

-- MA:TODO please check
evaluateSelected :: ProofData i => Strategy i i -> ProofTree (Selected i) -> TctM (ProofTree (Selected i))
evaluateSelected _ pt@(Open (Left _))          = return pt
evaluateSelected s (Open (Right p))            = fmap Right `fmap` evaluate s (Open p)
evaluateSelected _ (Failure r)                 = return (Failure r)
evaluateSelected s (Success n certfn subtrees) = Success n certfn <$> (evaluateSelected s `F.mapM` subtrees)


--- * state ----------------------------------------------------------------------------------------------------------

data St l where
  St :: ProofData l => ProofTree (Selected l) -> St l

unSt :: St l -> ProofTree (Selected l)
unSt (St pt) = pt

data a :+: b = a :+: b | Nil

null' :: (a :+: b) -> Bool
null' Nil = True
null' _   = False

head' :: (a :+: b) -> a
head' Nil       = error "Tct.Core.Interactive.head': empty list"
head' (a :+: _) = a

tail' :: (t :+: (a :+: b)) -> a :+: b
tail' Nil       = Nil
tail' (_ :+: b) = b

newtype State l = State { history_ :: l }

stateRef :: IORef (State (a :+: b))
stateRef = unsafePerformIO $ newIORef st
  where st = State { history_ = Nil }
{-# NOINLINE stateRef #-}

getState :: IO (State (a :+: b))
getState = readIORef stateRef

putState :: State (a :+: b) -> IO ()
putState = writeIORef stateRef

modifyState :: (State (a :+: b) -> State (c :+: d)) -> IO ()
modifyState f = getState >>= putState . f

initSt :: ProofData i => i -> IO ()
initSt st' = modifyState (\st -> st{ history_ = St (Open (Right st')) :+: Nil })

getSt :: IO (Maybe (St i))
getSt = do
  hst <- history_ `fmap` getState
  return $ if null' hst then Nothing else Just (head' hst)

maybeSt :: IO a -> (St i -> IO a) -> IO a
maybeSt a f = getSt >>= maybe a f

onSt :: (St i -> IO ()) -> IO ()
onSt = maybeSt (print "no problem specified")

putSt :: St i -> IO ()
putSt st' = do
  st <- readIORef stateRef
  writeIORef stateRef $ st { history_ = st' :+: history_ st }

modifySt :: ProofData o => (ProofTree (Selected i) -> ProofTree (Selected o)) -> IO ()
modifySt f = onSt (putSt . St . f . unSt)


--- * interface ------------------------------------------------------------------------------------------------------

-- | Load a problem.
load :: ProofData i => (FilePath -> IO (Either String i)) -> FilePath -> IO ()
load p  fp = do
  ret <- runErroneousIO $ tryIO (p fp) >>= liftEither . either (Left . TctProblemParseError) Right
  either print (\prob -> initSt prob >> print "Problem loaded." >> printState) ret

-- | Like 'load' but extracts the parser from a configuration.
--
-- > load' = load . parseProblem
load' :: ProofData i => TctConfig i -> FilePath -> IO ()
load' = load . parseProblem

-- | Modifies all sub-problems.
modifyProblems :: ProofData i => (i -> i) -> IO ()
modifyProblems = modifySt . fmap . fmap

-- | Performs an IO action on selected sub-problems.
onProblems :: (i -> IO()) -> IO ()
onProblems f = onSt $ F.mapM_ f . rights . F.toList . unSt

-- | Performs an IO action on the proof tree.
onTree :: (ProofTree i -> IO()) -> IO ()
onTree f = onSt $ f . removeSelection . unSt


-- | Select a list of sub-problems.
select :: [Int] -> IO ()
select is = onSt $ \(St l) -> putSt (St (selectLeafs is l)) >> printState

-- | Select all sub-problems.
selectAll :: IO ()
selectAll = onSt $ \(St l) -> putSt (St (selectAllLeafs l)) >> printState

apply' :: ProofData o => (Strategy i o -> ProofTree (Selected i) -> TctM (ProofTree (Selected o))) -> Strategy i o -> IO ()
apply' eval str = onSt $ \st -> do
  let t1 = unSt st
  t2 <- runInteractive (eval str t1)
  if not (isFailure t2) && size t2 > size t1
    then putSt (St t2) >> printState >> print "progressed :)"
    else print "no progress :/"

-- | Applies a strategy on all sub-problems.
apply :: ProofData o => Strategy i o -> IO ()
apply = apply' evaluateAll

-- | Applies a strategy on \selected\ sub-problems. Requires the strategy to have the same input-output type.
applySelected :: ProofData i => Strategy i i -> IO ()
applySelected = apply' evaluateSelected


-- | Undos state. Restores the prooftree before the last application of 'apply' and 'applySelected'.
undo :: IO ()
undo = do
  hst <- history_ `fmap` getState
  unless (null' hst) $ modifyState (\st -> st { history_ = tail' hst })
  printState

-- | Returns the the initial problem.
reset :: IO ()
reset = reset' >> printState where
  reset' = do
    hst <- history_ `fmap` getState
    case hst of
      Nil           -> return ()
      p@(_ :+: Nil) -> modifyState (\st -> st { history_ = p })
      _             -> modifyState (\st -> st { history_ = tail' hst }) >> reset'

-- | Print the proof.
printProof :: IO ()
printProof = onSt (PP.putPretty . pp)
  where pp (St l) = ppProofTree PP.pretty l

-- | Print detailed proof (including NoProgress nodes).
printDetailedProof :: IO ()
printDetailedProof = onSt (PP.putPretty . pp)
  where pp (St l) = ppDetailedProofTree PP.pretty l

-- | Print state, ie. a list of selected sub-problems.
printState :: IO ()
printState = onSt (PP.putPretty . pp)
  where pp (St l) = ppProofTreeLeafs PP.pretty l

