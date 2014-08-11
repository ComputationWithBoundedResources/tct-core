module Tct 
where


import qualified Config.Dyre as Dyre
import           Control.Monad.Error (ErrorT, runErrorT)
import           Control.Monad.Reader (runReaderT)
import           System.FilePath ((</>))
import           System.Directory (getHomeDirectory)
import qualified System.Time as Time

import           Tct.Options
import           Tct.Core (TctM, TctROState(..), runTct, Strategy)


class TctMode mode where
  type ModeProblem mode :: *
  parseProblem_         :: mode -> String -> ModeProblem mode
  strategies_           :: mode -> [Strategy (ModeProblem mode)]
  defaultStrategy_      :: mode -> Strategy (ModeProblem mode)
  modeOptions_          :: mode -> Options () (ModeProblem mode)


data Void = Void

instance TctMode Void where
  type ModeProblem Void = Void
  parseProblem_        = undefined
  strategies_          = undefined
  defaultStrategy_     = undefined
  modeOptions_         = undefined

type ErroneousIO = ErrorT String IO

runErroneousIO :: ErroneousIO a -> IO (Either String a)
runErroneousIO = runErrorT

data TctConfig prob = TctConfig 
  { satSolver       :: FilePath
  , smtSolver       :: FilePath
  , errMsg          :: String

  , parseProblem    :: String -> prob
  , strategies      :: [Strategy prob]
  , defaultStrategy :: Strategy prob
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

realMain :: TctConfig prob -> IO ()
realMain cfg = do
  r <- runErroneousIO $
    return cfg
  case r of
    Left s     -> putStrLn s
    Right cfg' -> print $ errMsg cfg' 

tctl :: TctConfig prob -> IO ()
tctl = Dyre.wrapMain $ Dyre.defaultParams
  { Dyre.projectName = "tctl"
  , Dyre.realMain    = realMain
  , Dyre.configDir   = Just tctldir
  , Dyre.cacheDir    = Just tctldir
  , Dyre.showError   = \cfg emsg -> cfg { errMsg = emsg }
  , Dyre.ghcOpts     = ["-threaded", "-package tctl"] }
  where tctldir = getHomeDirectory >>= \home -> return (home </> "tctl") 

