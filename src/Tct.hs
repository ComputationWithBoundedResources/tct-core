module Tct
where


import qualified Config.Dyre as Dyre
import           Control.Applicative
import Data.Monoid
import           Control.Monad
import           Control.Monad.Reader (runReaderT)
import           System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import           System.Directory (getHomeDirectory)
import qualified System.Time as Time
import System.Exit (exitSuccess, exitFailure)

import qualified Options.Applicative as O

import           Tct.Error
import           Tct.Core
import           Tct.Processors.Combinators
import           Tct.Pretty (Pretty, string, pretty, display)


-- TODO
-- get rid of redundancy in TctOptions, TctConfig


data TctMode prob opt = ProofData prob => TctMode
  { modeParser            :: String -> Either TctError prob
  , modeStrategies        :: [SomeProcessor prob]
  , modeDefaultStrategy   :: SomeProcessor prob
  , modeOptions           :: O.Parser opt
  , modeModifyer          :: prob -> opt -> prob }


data Void = Void deriving (Show, Read)

instance Pretty Void where pretty = const $ string "Void"

void :: TctMode Void Void
void = TctMode
  { modeParser          = const $ Right Void
  , modeStrategies      = []
  , modeDefaultStrategy = SomeProc $ FailProc
  , modeOptions         = pure Void
  , modeModifyer        = const id }

data TctOptions m = TctOptions
  { satSolver_    :: Maybe FilePath
  , smtSolver_    :: Maybe FilePath
  , modeOptions_  :: m
  , strategyName_ :: Maybe String
  , problemFile_  :: FilePath
  }


mkParser :: O.Parser m -> O.ParserInfo (TctOptions m)
mkParser mparser = O.info (versioned <*> O.helper <*> tctp) desc
  where
    versioned = O.infoOption "version" $ O.long "version" <> O.short 'v' <> O.help "Show Version" <> O.hidden
    tctp = TctOptions
      <$> O.optional (O.strOption (O.long "satPath" <> O.help "Set path to minisat."))
      <*> O.optional (O.strOption (O.long "smtPath" <> O.help "Set path to minismt."))
      -- <*> O.subparser (O.command "mode" (O.info mparser O.idm))
      <*> mparser
      <*> O.optional (O.strOption (O.long "strategy" <> O.short 's' <> O.help "The strategy to apply."))
      <*> O.argument O.str (O.metavar "File")
    desc = O.briefDesc <> O.progDesc "TcT -- Tyrolean Complexity Tool"

data TctConfig prob = TctConfig
  { satSolver       :: FilePath
  , smtSolver       :: FilePath
  , strategies      :: [SomeProcessor prob]
  , defaultStrategy :: SomeProcessor prob
  }


defaultStrategies :: ProofData prob => [SomeProcessor prob]
defaultStrategies =
  [ SomeProc $ TimeoutProc Nothing Nothing FailProc
  ]

defaultTctConfig :: ProofData prob => TctMode prob opt -> TctConfig prob
defaultTctConfig mode = TctConfig
  { satSolver       = "/usr/bin/minisat"
  , smtSolver       = "/usr/bin/yices"
  , strategies      = defaultStrategies ++ modeStrategies mode
  , defaultStrategy = modeDefaultStrategy mode
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

realMain :: ProofData prob => TctModeConfig prob opt -> IO ()
realMain dcfg = do
  r <- runErroneousIO $ do
    mode <- liftEither dcfg
    let
      TctMode
        { modeParser            = theProblemParser
        , modeDefaultStrategy   = theDefaultStrategy
        , modeOptions           = theOptionParser
        , modeModifyer          = theModifyer
        } = mode
    opts <- liftIO $ O.execParser (mkParser theOptionParser)
    let
      cfg = defaultTctConfig mode
      TctOptions
        { strategyName_ = theStrategyName
        , problemFile_  = theProblemFile
        , modeOptions_  = theOptions
        } = opts
    file  <- liftIO $ readFile theProblemFile
    prob  <- liftEither $ theProblemParser file >>= \prob -> return (theModifyer prob theOptions)
    strat <- maybe (return theDefaultStrategy) (liftEither . readAnyProc (strategies cfg)) theStrategyName
    pt    <- liftIO $ fromReturn `liftM` (run cfg $ evaluate (Proc $ strat) prob)
    liftIO $ do
      print $ strategies cfg
      putStrLn "Problem:"
      putStrLn . display $ pretty prob
      putStrLn "ProofTree:"
      putStrLn . display $ pretty pt
      putStrLn "Certificate:"
      putStrLn . display $ pretty $ certificate pt
  case r of
    Left err -> hPutStrLn stderr (show err) >> exitFailure
    Right _  -> exitSuccess


applyMode :: TctMode prob opt ->TctModeConfig prob opt
applyMode = Right

type TctModeConfig prob opt = Either TctError (TctMode prob opt)

tctl :: ProofData prob => TctModeConfig prob opt -> IO ()
tctl = Dyre.wrapMain $ Dyre.defaultParams
  { Dyre.projectName = "tctl"
  , Dyre.realMain    = realMain
  , Dyre.configDir   = Just tctldir
  , Dyre.cacheDir    = Just tctldir
  , Dyre.showError   = \_ emsg -> Left (TctDyreError emsg)
  , Dyre.ghcOpts     = ["-threaded", "-package tct-3.0"] }
  --, Dyre.ghcOpts     = ["-threaded"] }
  where tctldir = getHomeDirectory >>= \home -> return (home </> "tctl")

