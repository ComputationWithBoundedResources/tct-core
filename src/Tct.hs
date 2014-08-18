module Tct 
where


import qualified Config.Dyre as Dyre
import           Control.Monad (liftM)
import           Control.Monad.Error (ErrorT, runErrorT)
import           Control.Monad.Reader (runReaderT)
import           System.Environment (getArgs)
import           System.FilePath ((</>))
import           System.Directory (getHomeDirectory)
import qualified System.Time as Time

import           Tct.Options
import           Tct.Core
import           Tct.Processors.Combinators
import           Tct.Pretty (Pretty, string, pretty, display)


class TctMode mode where
  type ModeProblem mode :: *
  parseProblem_         :: mode -> String -> ModeProblem mode
  strategies_           :: mode -> [SomeProcessor (ModeProblem mode)]
  defaultStrategy_      :: mode -> SomeProcessor (ModeProblem mode)
  modeOptions_          :: mode -> Options () (ModeProblem mode)

data Void = Void deriving (Show, Read)

instance Pretty Void where pretty = const $ string "Void"

instance TctMode Void where
  type ModeProblem Void = Void
  parseProblem_          = const read
  strategies_            = const []
  defaultStrategy_       = const $ SomeProc failP
  modeOptions_           = const []

type ErroneousIO = ErrorT String IO

runErroneousIO :: ErroneousIO a -> IO (Either String a)
runErroneousIO = runErrorT

data TctConfig prob = TctConfig 
  { satSolver       :: FilePath
  , smtSolver       :: FilePath
  , errMsg          :: String

  , parseProblem    :: String -> prob
  , strategies      :: [SomeProcessor prob]
  , defaultStrategy :: SomeProcessor prob
  , modeOptions     :: Options () prob
  }

defaultTctConfig :: TctMode mode => mode -> TctConfig (ModeProblem mode)
defaultTctConfig mode = TctConfig 
  { satSolver    = "/usr/bin/minisat"
  , smtSolver    = "/usr/bin/yices"
  , errMsg       = ""

  , parseProblem    = parseProblem_ mode
  , strategies      = strategies_ mode
  , defaultStrategy = defaultStrategy_ mode
  , modeOptions     = modeOptions_ mode
  }
  
run :: TctConfig prob -> TctM a -> IO a
run cfg m = do 
  time <- Time.getClockTime
  let 
    state = TctROState 
      { satSolverExe = satSolver cfg
      , smtSolverExe = smtSolver cfg
      , startTime    = time
      , stopTime     = Nothing }
  runReaderT (runTct m) state

realMain :: (ProofData prob) => TctConfig prob -> IO ()
realMain cfg = do
  (st:fn:_) <- getArgs
  let ps1 = strategies cfg
      ps2 = SomeProc (timeoutIn 20 (SomeProc failP))
      ps3 = ps2 : ps1
  prb <- parseProblem cfg `liftM` readFile fn
  print prb
  print ps3
  let 
    pr = either (\err -> error ("not found " ++ err)) id (readAnyProc ps3 st)
    --pr = fromString ps3 st
  
  pt <- fromReturn `liftM` (run cfg $ evaluate (Proc $  pr) prb)
  --pt <- fromReturn `liftM` (run cfg $ evaluate (Proc $ defaultStrategy cfg) prb)
  putStrLn $ "ProofTree:"
  putStrLn . display $ pretty pt
  putStrLn $ "Certificate:"
  putStrLn . display . pretty $ certificate pt
  r <- runErroneousIO $
    return cfg
  case r of
    Left s     -> putStrLn s
    Right cfg' -> print $ errMsg cfg' 

tctl :: (ProofData prob) => TctConfig prob -> IO ()
tctl = Dyre.wrapMain $ Dyre.defaultParams
  { Dyre.projectName = "tctl"
  , Dyre.realMain    = realMain
  , Dyre.configDir   = Just tctldir
  , Dyre.cacheDir    = Just tctldir
  , Dyre.showError   = \cfg emsg -> cfg { errMsg = emsg }
  , Dyre.ghcOpts     = ["-threaded", "-package tct-3.0"] }
  --, Dyre.ghcOpts     = ["-threaded"] }
  where tctldir = getHomeDirectory >>= \home -> return (home </> "tctl") 

