module Tct.Core
where

import           Control.Applicative (Applicative)
import qualified Control.Concurrent.Async as Async
import           Control.Monad.Error (catchError, MonadError)
import           Control.Monad.Reader (liftIO, MonadIO, ask, local, runReaderT, MonadReader, ReaderT)
import           Data.Foldable as F
import           Data.Traversable as T
import qualified System.Time as Time
import qualified System.Timeout as Timeout
import qualified Tct.Certificate as C
import qualified Tct.Core.Options as O
import qualified Tct.Pretty as PP
import qualified Tct.Problem as P
import qualified Tct.Xml as Xml

data TctROState = 
    TctROState { solverExe :: FilePath
               , startTime :: Time.ClockTime
               , stopTime :: Maybe Time.ClockTime}

data TctConfig = TctConfig { satSolver :: FilePath }
               

newtype TctM r = TctM { runTct :: ReaderT TctROState IO r}
    deriving (Monad, Applicative, MonadIO, MonadReader TctROState, Functor, MonadError IOError)


run :: TctConfig -> TctM a -> IO a
run cfg m = do 
  time <- Time.getClockTime
  let state = TctROState { solverExe = satSolver cfg
                         , startTime = time
                         , stopTime = Nothing }
  runReaderT (runTct m) state

askState :: TctM (TctROState)
askState = ask

toIO :: TctM a -> TctM (IO a)
toIO m = runReaderT (runTct m) `fmap` askState

async :: TctM a -> TctM (Async.Async a)
async m = toIO m >>= liftIO . Async.async

wait :: Async.Async a -> TctM a
wait = liftIO . Async.wait

timeout :: Int -> TctM a -> TctM (Maybe a)
timeout n m = toIO m' >>= liftIO . Timeout.timeout n
    where 
      m' = do 
        Time.TOD sec pico <- liftIO Time.getClockTime 
        let newTime = Just (Time.TOD (sec + toInteger n) pico)
        local (\ r -> r { stopTime = min newTime (stopTime r) }) m



type Fork t = (Foldable t, Functor t, Traversable t)
type ProofData d = (Xml.Xml d, PP.Pretty d, Show d)

newtype Id a = Id a
    deriving (Foldable, Functor, Traversable)

type CertificateFn p = Forking p C.Certificate -> C.Certificate


data Result p = 
    Fail { proofData :: ProofObject p }
  | Success { subProblems :: Forking p P.Problem
            , proofData :: ProofObject p
            , certificateFn :: CertificateFn p }

class (Show p, ProofData (ProofObject p), Fork (Forking p))  => Processor p where
    type ProofObject p :: *
    type Forking p :: * -> *
    type Forking p = Id
    name :: p -> String
    options :: p -> O.Options [SomeProcessor] p
    options = const []
    solve :: p -> P.Problem -> TctM (Result p)

data SomeProcessor where
    SomeProcessor :: Processor p => p -> SomeProcessor


data ErroneousProof p = ErroneousProof IOError p deriving Show

instance Processor p => Xml.Xml (ErroneousProof p) where
    toXml (ErroneousProof err p) = 
        Xml.elt "error" [] [ Xml.elt "processor" [] [Xml.text (name p)]
                           , Xml.elt "message" [] [Xml.text (show err)] ]

instance Processor p => PP.Pretty (ErroneousProof p) where 
    pretty (ErroneousProof err p) = 
        PP.text "Processor" PP.<+> PP.squotes (PP.text (name p)) 
        PP.<+> PP.text "signalled the following error:"
        PP.<$$> PP.indent 2 (PP.paragraph (show err))

data ErroneousP p = ErroneousP IOError p deriving Show

instance Processor p => Processor (ErroneousP p) where
    type ProofObject (ErroneousP p) = ErroneousProof p
    name (ErroneousP err p) = name p ++ "[error: " ++ take 10 (show err) ++ "]"
    solve (ErroneousP err p) _ = return (Fail (ErroneousProof err p))



data ProofNode p = 
    ProofNode { problem :: P.Problem
              , processor :: p
              , proof :: ProofObject p}
                 
data ProofTree l where 
    Open :: l -> ProofTree l
    NoProgress :: Processor p => ProofNode p -> ProofTree l -> ProofTree l
    Progress :: Processor p => ProofNode p -> CertificateFn p -> Forking p (ProofTree l) -> ProofTree l


children :: ProofTree l -> [ProofTree l]
children (Open _) = []
children (NoProgress _ t) = [t]
children (Progress _ _ subtrees) = toList subtrees

certificate :: ProofTree l -> C.Certificate
certificate Open {} = C.unbounded
certificate (NoProgress _ subtree) = certificate subtree
certificate (Progress _ certfn subtrees) = certfn (fmap certificate subtrees)

open :: ProofTree l -> [l]
open (Open l) = [l]
open tree = open `Prelude.concatMap` children tree

isOpen :: ProofTree l -> Bool
isOpen = null . open

isClosed :: ProofTree l -> Bool
isClosed = not . isOpen



data TctStatus = 
    TctStatus { currentProblem :: P.Problem
              , runningTime :: Int
              , remainingTime :: Maybe Int }

askStatus :: P.Problem -> TctM TctStatus
askStatus prob = do
    st <- askState
    now <- liftIO $ Time.getClockTime
    return TctStatus { currentProblem = prob
                     , runningTime = Time.tdSec (Time.diffClockTimes now (startTime st))
                     , remainingTime = (Time.tdSec . flip Time.diffClockTimes now) `fmap` stopTime st}
       

data Strategy = 
    Proc SomeProcessor 
   | Trying Bool Strategy
   | Strategy :>>>: Strategy
   | Strategy :>||>: Strategy
   | Strategy :<>: Strategy
--   | Repeat Strategy
   | WithStatus (TctStatus -> Strategy)

try :: Strategy -> Strategy 
try s@(Trying _ _) = (Trying True s)
try (WithStatus f) = WithStatus (try . f)
try s = Trying True s

force :: Strategy -> Strategy 
force s@(Trying _ _) = (Trying False s)
force (WithStatus f) = WithStatus (force . f)
force s = Trying False s


-- repeat :: Strategy -> Strategy 
-- repeat s@(Repeat _) = s
-- repeat s = s

(>>>) :: Strategy -> Strategy -> Strategy
(>>>) = (:>>>:)

(>||>) :: Strategy -> Strategy -> Strategy
(>||>) = (:>||>:)

(<>) :: Strategy -> Strategy -> Strategy
(<>) = (:<>:)

withState :: (TctStatus -> Strategy) -> Strategy 
withState = WithStatus

progress :: ProofTree l -> Bool 
progress (Open _) = False
progress (NoProgress _ subtree) = progress subtree
progress (Progress {}) = True

resultToTree :: Processor p => P.Problem -> p -> Result p -> ProofTree P.Problem
resultToTree prob p (Fail po) = 
    NoProgress (ProofNode prob p po) (Open prob)
resultToTree prob p (Success subprobs po certfn) = 
    Progress (ProofNode prob p po) certfn (Open `fmap` subprobs)

data Return l = Continue l | Abort l

evaluate :: Strategy -> P.Problem -> TctM (Return (ProofTree P.Problem))
evaluate (Proc (SomeProcessor p)) prob = (f `fmap` solve p prob) `catchError` errNode
    where 
      f res@(Fail {}) = Abort (resultToTree prob p res)
      f res@(Success {}) = Continue (resultToTree prob p res)
      errNode err = evaluate (Proc (SomeProcessor (ErroneousP err p))) prob

evaluate (Trying True s) prob = f `fmap` evaluate s prob
    where 
      f (Abort pt) = Continue pt 
      f pt = pt

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

liftProgress :: Processor p => ProofNode p -> CertificateFn p-> Forking p (Return (ProofTree l)) -> Return (ProofTree l)
liftProgress n certfn rs
    | F.any isAbort (toList rs) = Abort tree
    | otherwise = Continue tree
    where 
      tree = Progress n certfn (returnVal `fmap` rs)
      returnVal (Continue pt) = pt
      returnVal (Abort pt) = pt
      isAbort (Abort _) = True
      isAbort _         = False

evaluateTree :: Strategy -> ProofTree P.Problem -> TctM (Return (ProofTree P.Problem))
evaluateTree s (Open p) = evaluate s p
evaluateTree s (NoProgress n subtree) = liftNoProgress n `fmap` evaluateTree s subtree
evaluateTree s (Progress n certfn subtrees) = liftProgress n certfn `fmap` (evaluateTree s `T.mapM` subtrees)

evaluateTreePar :: Strategy -> ProofTree P.Problem -> TctM (Return (ProofTree P.Problem))
evaluateTreePar s t = spawnTree t >>= collect
    where 
      spawnTree :: ProofTree P.Problem -> TctM (ProofTree (Async.Async (Return (ProofTree P.Problem))))
      spawnTree (Open p) = Open `fmap` async (evaluate s p)
      spawnTree (NoProgress n subtree) = NoProgress n `fmap` spawnTree subtree
      spawnTree (Progress n certfn subtrees) = Progress n certfn `fmap` (spawnTree `T.mapM` subtrees)

      collect :: ProofTree (Async.Async (Return (ProofTree P.Problem))) -> TctM (Return (ProofTree P.Problem))
      collect (Open a) = wait a
      collect (NoProgress n subtree) = liftNoProgress n `fmap` collect subtree
                                       
      collect (Progress n certfn subtrees) = 
          liftProgress n certfn `fmap` (collect `T.mapM` subtrees)
        


    


