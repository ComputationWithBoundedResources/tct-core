-- | This module provides a basic interactive functionality via ghci.
-- NOTE: Totally unsafe at the moment.
module Tct.Core.Interactive 
  (
  load

  , reset
  , undo

  , select
  , unselect

  , apply
  , state
  , proof, proof'
  ) where


import           Control.Monad
import qualified Control.Monad.State.Strict as S
import qualified Data.Foldable              as F
import           Data.IORef
import qualified Data.Traversable           as F
import           System.IO.Unsafe

import           Tct.Core.Common.Error
import qualified Tct.Core.Common.Pretty     as PP
import           Tct.Core.Data              hiding (proof)
import           Tct.Core.Main

-- MS
-- * currently unsafe; we have to initialise IORef; use MVar or Maybe
-- * IORefs do not work well with polymorphic types; it works but we can not use
-- Proofdata constraints; currently I just put necessary functions into the state
-- this will not work if we extend strategy with problem transformations in the future


--- * selection ------------------------------------------------------------------------------------------------------

type Selected l = Either l l

select :: [Int] -> ProofTree (Selected l) -> ProofTree (Selected l)
select ns pt = S.evalState (F.mapM k pt) 0
  where
    k p = S.modify succ >> S.get >>= \i -> return (either (f i) (f i) p)
    f i = if i `elem` ns then Right else Left

unselect :: ProofTree (Selected l) -> ProofTree (Selected l)
unselect = fmap (either Right Right) where

evaluateSelected :: Strategy prob -> ProofTree (Selected prob) -> TctM (Return (ProofTree (Selected prob)))
evaluateSelected _ pt@(Open (Left _))           = return (Continue pt)
evaluateSelected s (Open (Right p))             = (fmap . fmap) Right `fmap` evaluate s p
evaluateSelected s (NoProgress n subtree)       = liftNoProgress n `fmap` evaluateSelected s subtree
evaluateSelected s (Progress n certfn subtrees) = liftProgress n certfn `fmap` (evaluateSelected s `F.mapM` subtrees)


newtype St l = St { unSt :: ProofTree (Selected l) }

{-onPt :: ProofData l => (ProofTree (Selected l1) -> ProofTree (Selected l)) -> St l1 -> St l-}
{-onPt f = St . f . unSt-}

{-eonSt :: ProofData l => (ProofTree (Selected l) -> a) -> St l -> a-}
{-eonSt f = f . unSt-}

data State l = State
  { current :: St l
  , history :: [St l]

  , pretty  :: l -> PP.Doc
  , parse   :: String -> Either TctError l }


stateRef :: IORef (State l)
stateRef = unsafePerformIO $ newIORef st
  where st = State { current = undefined, history = undefined, pretty = undefined, parse = undefined }
{-# NOINLINE stateRef #-}

getState :: IO (State l)
getState = readIORef stateRef

putState :: State l -> IO ()
putState = writeIORef stateRef

modifyState :: (State l -> State l) -> IO ()
modifyState f = getState >>= putState . f

getSt :: IO (St l)
getSt = current `fmap` getState

getPretty :: IO (l -> PP.Doc)
getPretty = pretty `fmap` getState


initSt :: l -> IO ()
initSt st' = modifyState (\st -> st{ current = St (Open (Right st')), history = [] })

putSt :: St l -> IO ()
putSt st' = do
  st <- readIORef stateRef
  writeIORef stateRef $ st { current = st', history = current st: history st }

reset :: IO ()
reset = do
  hst <- history `fmap` getState
  unless (null hst) $  modifyState (\st -> st { current = last hst, history = [] })

undo :: IO ()
undo = do
  hst <- history `fmap` getState
  unless (null hst) $ modifyState (\st -> st { current = head hst, history = tail hst })

{-modifySt :: (St l -> St l) -> IO ()-}
{-modifySt f = getSt >>= putSt . f-}


getProblems :: IO [l]
getProblems = do
  (St l) <- getSt
  return [ p | Right p <- F.toList l]

printState :: IO ()
printState = do
  pp <- pretty `fmap` getState
  ps <- getProblems
  putStrLn $ PP.display $ PP.enumerate (map pp ps)

{-loadMode :: ProofData l => TctMode l o -> IO () -}
{-loadMode m = modifyState $ \st -> st { pretty = PP.pretty, parse = modeParser m }-}

load :: ProofData l => TctMode l o -> FilePath -> IO ()
load m fp = do
  modifyState $ \st -> st { pretty = PP.pretty, parse = modeParser m }
  ret <- runErroneousIO $ do
    p  <- tryIO $ readFile fp
    pa <- liftIO $ parse `fmap` getState
    liftEither (pa p)
  either print (\prob -> initSt prob >> print "Problem loaded." >> printState) ret

apply :: Strategy prob -> IO ()
apply str = do
  st1 <- getSt
  ret <- run $ evaluateSelected str (unSt st1)
  returning
    (\s -> putSt (St s) >> printState)
    (const $ print "no progress :/") ret

proof :: IO ()
proof = do
  pp <- getPretty
  st <- getSt
  PP.putPretty $ ppProofTree pp (unSt st)

proof' :: IO ()
proof' = do
  pp <- getPretty
  st <- getSt
  PP.putPretty $ ppDetailedProofTree pp (unSt st)

state :: IO ()
state = printState

