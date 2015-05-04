-- | This module provides a basic interactive functionality via ghci.
module Tct.Core.Interactive
  (
  load
  , select
  , unselect

  , modifyProblem
  , onProblem
  , apply

  , reset
  , undo

  , state
  , proof
  ) where


import           Control.Monad
import qualified Control.Monad.State.Strict as S
import           Data.Either                (rights)
import qualified Data.Foldable              as F
import           Data.IORef
import qualified Data.Traversable           as F
import           System.IO.Unsafe

import           Tct.Core.Common.Error
import qualified Tct.Core.Common.Pretty     as PP
import           Tct.Core.Data              hiding (proof)
import           Tct.Core.Main


--- * selection ------------------------------------------------------------------------------------------------------

type Selected l = Either l l

instance PP.Pretty l => PP.Pretty (Selected l) where
  pretty (Left _)  = PP.empty
  pretty (Right l) = PP.pretty l

selectLeafs :: ProofData l => [Int] -> ProofTree (Selected l) -> ProofTree (Selected l)
selectLeafs ns pt = S.evalState (F.mapM k pt) 0
  where
    k p = S.modify succ >> S.get >>= \i -> return (either (f i) (f i) p)
    f i = if i `elem` ns then Right else Left

unselectLeafs :: ProofTree (Selected l) -> ProofTree (Selected l)
unselectLeafs = fmap (either Right Right) where

evaluateSelected :: Strategy i i -> ProofTree (Selected i) -> TctM (Return (ProofTree (Selected i)))
evaluateSelected _ pt@(Open (Left _))           = return (Continue pt)
evaluateSelected s (Open (Right p))             = (fmap . fmap) Right `fmap` evaluate s p
evaluateSelected s (NoProgress n subtree)       = liftNoProgress n `fmap` evaluateSelected s subtree
evaluateSelected s (Progress n certfn subtrees) = liftProgress n certfn `fmap` (evaluateSelected s `F.mapM` subtrees)


--- * state ----------------------------------------------------------------------------------------------------------

data St l where
  St :: ProofData l => ProofTree (Selected l) -> St l

unSt :: St l -> ProofTree (Selected l)
unSt (St pt) = pt

newtype State l = State { history_ :: [St l] }

stateRef :: IORef (State l)
stateRef = unsafePerformIO $ newIORef st
  where st = State { history_ = [] }
{-# NOINLINE stateRef #-}

getState :: IO (State l)
getState = readIORef stateRef

putState :: State l -> IO ()
putState = writeIORef stateRef

modifyState :: (State l -> State l) -> IO ()
modifyState f = getState >>= putState . f

initSt :: ProofData l => l -> IO ()
initSt st' = modifyState (\st -> st{ history_ = [St (Open (Right st'))] })

getSt :: IO (Maybe (St l))
getSt = do
  hst <- history_ `fmap` getState
  return $ if null hst then Nothing else Just (head hst)

maybeSt :: IO a -> (St l -> IO a) -> IO a
maybeSt a f = getSt >>= maybe a f

onSt :: (St l -> IO ()) -> IO ()
onSt = maybeSt (print "no problem specified")

putSt :: St l -> IO ()
putSt st' = do
  st <- readIORef stateRef
  writeIORef stateRef $ st { history_ = st': history_ st }

modifySt :: ProofData l => (ProofTree (Selected l) -> ProofTree (Selected l)) -> IO ()
modifySt f = onSt (putSt . St . f . unSt)

printState :: IO ()
printState = onSt (PP.putPretty . pp)
  where pp (St l) = ppProofTreeLeafes PP.pretty l


--- * interface ------------------------------------------------------------------------------------------------------

load :: ProofData l => TctMode l l o -> FilePath -> IO ()
load m fp = do
  ret <- runErroneousIO $ do
    p  <- tryIO $ readFile fp
    liftEither (modeParser m p)
  either print (\prob -> initSt prob >> print "Problem loaded." >> printState) ret

modifyProblem :: ProofData l => (l -> l) -> IO ()
modifyProblem = modifySt . fmap . fmap

onProblem :: (l -> IO ()) -> IO ()
onProblem f = onSt $ F.mapM_ f . rights . F.toList . unSt

select :: [Int] -> IO ()
select is = onSt $ \(St l) -> putSt (St (selectLeafs is l)) >> printState

unselect :: IO ()
unselect = onSt $ \(St l) -> putSt (St (unselectLeafs l)) >> printState

apply :: ProofData i => Strategy i i -> IO ()
apply str = onSt $ \st -> do
  ret <- run $ evaluateSelected str (unSt st)
  -- MS: FIXME should be isProgressing and som more informative output
  returning
    (\s -> putSt (St s) >> printState)
    (const $ print "no progress :/")
    (print "no progress :/")
    ret

proof :: IO ()
proof = onSt (PP.putPretty . pp)
  where pp (St l) = ppProofTree PP.pretty l

state :: IO ()
state = printState

undo :: IO ()
undo = do
  hst <- history_ `fmap` getState
  unless (null hst) $ modifyState (\st -> st { history_ = tail hst })
  printState

reset :: IO ()
reset = do
  hst <- history_ `fmap` getState
  unless (null hst) $ modifyState (\st -> st { history_ = [last hst] })
  printState

