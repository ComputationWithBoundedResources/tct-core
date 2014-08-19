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


class ProofData (ModeProblem mode) => TctMode mode where
  type ModeProblem mode :: *
  type ModeOptions mode :: *
  modeParser            :: mode -> String -> Either TctError (ModeProblem mode)
  modeStrategies        :: mode -> [SomeProcessor (ModeProblem mode)]
  modeDefaultStrategy   :: mode -> SomeProcessor (ModeProblem mode)
  modeOptions           :: mode -> O.Parser (ModeOptions mode)
  modeModifyer          :: mode -> modeOptions -> ModeProblem mode -> ModeProblem mode

data Void = Void deriving (Show, Read)

instance Pretty Void where pretty = const $ string "Void"

instance TctMode Void where
  type ModeProblem Void = Void
  type ModeOptions Void = Void
  modeParser _ _        = Right Void
  modeStrategies _      = []
  modeDefaultStrategy _ = SomeProc $ FailProc
  modeOptions _         = pure Void
  modeModifyer _ _      = id
  




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
      <*> O.optional (O.strOption (O.long "strategy" <> O.help "The strategy to apply."))
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

defaultTctConfig :: TctMode mode => mode -> TctConfig (ModeProblem mode)
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

realMain :: TctMode mode => TctModeConfig mode -> IO ()
realMain dcfg = do
  r <- runErroneousIO $ do
    mode <- liftEither dcfg
    opts <- liftIO $ O.execParser (mkParser (modeOptions mode))
    let
      cfg = defaultTctConfig mode
      TctOptions
        { strategyName_ = strategyNameM
        , problemFile_  = problemFile
        } = opts

    prob <- liftEither $ modeModifyer mode (modeOptions_ opts)`liftM` modeParser mode problemFile
    strat <- maybe (return $ modeDefaultStrategy mode) (liftEither . readAnyProc (strategies cfg)) strategyNameM
    pt <- liftIO $ fromReturn `liftM` (run cfg $ evaluate (Proc $ strat) prob)
    liftIO $ do
      putStrLn "Problem:"
      putStrLn . display $ pretty prob
      putStrLn "ProofTree:"
      putStrLn . display $ pretty pt
      putStrLn "Certificate:"
      putStrLn . display $ pretty $ certificate pt
  case r of
    Left err -> hPutStrLn stderr (show err) >> exitFailure
    Right _  -> exitSuccess



  --(st:fn:_) <- getArgs
  --let ps1 = strategies cfg
      --ps2 = SomeProc (timeoutIn 20 (SomeProc failP))
      --ps3 = ps2 : ps1
  --prb <- parseProblem cfg `liftM` readFile fn
  --print prb
  --print ps3
  --let 
    --pr = either (\err -> error ("not found " ++ err)) id (readAnyProc ps3 st)
    ----pr = fromString ps3 st
  
  --pt <- fromReturn `liftM` (run cfg $ evaluate (Proc $  pr) prb)
  ----pt <- fromReturn `liftM` (run cfg $ evaluate (Proc $ defaultStrategy cfg) prb)
  --putStrLn $ "ProofTree:"
  --putStrLn . display $ pretty pt
  --putStrLn $ "Certificate:"
  --putStrLn . display . pretty $ certificate pt

applyMode :: TctMode mode => mode -> TctModeConfig mode
applyMode = Right 

type TctModeConfig mode = Either TctError mode

tctl :: TctMode mode => TctModeConfig mode -> IO ()
tctl = Dyre.wrapMain $ Dyre.defaultParams
  { Dyre.projectName = "tctl"
  , Dyre.realMain    = realMain
  , Dyre.configDir   = Just tctldir
  , Dyre.cacheDir    = Just tctldir
  , Dyre.showError   = \_ emsg -> Left (TctDyreError emsg)
  , Dyre.ghcOpts     = ["-threaded", "-package tct-3.0"] }
  --, Dyre.ghcOpts     = ["-threaded"] }
  where tctldir = getHomeDirectory >>= \home -> return (home </> "tctl") 

