{- | This module provides the user interaction.

-}
module Tct
  ( 
  TctMode (..)
  , applyMode
  , void
  , version
  )
  where


import qualified Config.Dyre                as Dyre (Params (..), defaultParams, wrapMain)
import           Control.Applicative        (pure, (<$>), (<*>))
import           Control.Monad              (liftM)
import           Control.Monad.Reader       (runReaderT)
import           Data.Monoid                (mconcat)
import qualified Options.Applicative        as O
import           System.Directory           (getHomeDirectory)
import           System.Exit                (exitFailure, exitSuccess)
import           System.FilePath            ((</>))
import           System.IO                  (hPrint, stderr)
import qualified System.Time                as Time

import           Tct.Core
import           Tct.Common.Error
import qualified Tct.Common.Pretty                 as PP
import           Tct.Combinators


-- TODO: 
-- get rid of redundancy in TctOptions, TctConfig
-- currently only modeoptions can be constructed in conf file
-- so realmain should hava type TctConfig prob opt -> IO ()

-- | 'TctMode' provides all infromation necesary to construct a Tct customised for a problem type.
data TctMode prob opt = TctMode
  { modeParser          :: String -> Either TctError prob -- ^ The parser for the problem.
  , modeStrategies      :: [SomeParsableProcessor prob]   -- ^ Problem specific parsable Processor/Strategies.
                                                          --   These are added to default 'processors'.
  , modeDefaultStrategy :: Strategy prob                  -- ^ The default strategy to execute.
  , modeOptions         :: O.Parser opt                   -- ^ Problem specific option parser. 
                                                          --   These are added to the standard Tct options.
  , modeModifyer        :: prob -> opt -> prob            -- ^ This function is applied to the initial problem, 
                                                          --   using the options parsed from command line.
  }

-- | Construct customised Tct mode. Example usage.
--
-- > main = tctl $ applyMode void
applyMode :: ProofData prob => TctMode prob opt -> IO ()
applyMode = tctl . Right

data Void = Void deriving (Show, Read)
instance PP.Pretty Void where pretty = const $ PP.string "Void"

-- | An example 'TctMode'.
void :: TctMode Void Void
void = TctMode
  { modeParser          = const $ Right Void
  , modeStrategies      = []
  , modeDefaultStrategy = Proc abort
  , modeOptions         = pure Void
  , modeModifyer        = const id }


data TctOptions m = TctOptions
  { 
  -- satSolver_    :: Maybe FilePath
  -- , smtSolver_    :: Maybe FilePath
    modeOptions_  :: m
  , strategyName_ :: Maybe String
  , problemFile_  :: FilePath
  }


mkParser :: [SomeParsableProcessor proc] -> O.Parser m -> O.ParserInfo (TctOptions m)
mkParser ps mparser = O.info (versioned <*> listed <*> O.helper <*> tctp) desc
  where
    listed = O.infoOption (PP.display $ mkDescription ps) $ mconcat [O.long "list", O.help "Display list of strategies."]
    versioned = O.infoOption version  $ mconcat [O.long "version", O.short 'v', O.help "Display Version.",  O.hidden]
    tctp = TctOptions
      -- <$> O.optional (O.strOption (mconcat [O.long "satPath", O.help "Set path to minisat."]))
      -- <*> O.optional (O.strOption (mconcat [O.long "smtPath", O.help "Set path to minismt."]))
      <$> mparser
      <*> O.optional (O.strOption (mconcat [O.long "strategy", O.short 's', O.help "The strategy to apply."]))
      <*> O.argument O.str (O.metavar "File")
    desc = mconcat
      [ O.headerDoc   . Just $ PP.string "TcT -- Tyrolean Complexity Tool"
      , O.progDescDoc . Just $ PP.string owl
      , O.footerDoc   . Just $ PP.string "version" PP.<+> PP.string version PP.<> PP.char ',' PP.<+> PP.string licence
      ]
        

-- | Current version.
version :: String
version = "3.0.0"

licence :: String
licence = "some licence"
    
owl :: String
owl = unlines
  [ " ,___, "
  , " [O.o]   - TcT is a transformer framework for automated complexity analysis."
  , "/)___) " 
  , "--\"-\"-"
  ]


data TctConfig prob = TctConfig
  { satSolver       :: FilePath
  , smtSolver       :: FilePath
  , strategies      :: [SomeParsableProcessor prob]
  , defaultStrategy :: Strategy prob
  }


defaultStrategies :: ProofData prob => [SomeParsableProcessor prob]
defaultStrategies = parsableProcessors

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
    opts <- liftIO $ O.execParser (mkParser (defaultStrategies ++ modeStrategies mode) theOptionParser)
    let
      cfg = defaultTctConfig mode
      TctOptions
        { strategyName_ = theStrategyName
        , problemFile_  = theProblemFile
        , modeOptions_  = theOptions
        } = opts
    file  <- tryIO $ readFile theProblemFile
    prob  <- liftEither $ theProblemParser file >>= \prob -> return (theModifyer prob theOptions)
    st    <- maybe (return theDefaultStrategy) (liftEither . liftM Proc . parseSomeParsableProcessor (strategies cfg)) theStrategyName
    pt    <- liftIO $ fromReturn `liftM` run cfg (evaluate st prob)
    liftIO $ do
      print $ strategies cfg
      putStrLn "Problem:"
      putStrLn . PP.display $ PP.pretty prob
      putStrLn "ProofTree:"
      putStrLn . PP.display $ PP.pretty pt
      putStrLn "Certificate:"
      putStrLn . PP.display $ PP.pretty $ certificate pt
  case r of
    Left err -> hPrint stderr err >> exitFailure
    Right _  -> exitSuccess

type TctModeConfig prob opt = Either TctError (TctMode prob opt)

tctl :: ProofData prob => TctModeConfig prob opt -> IO ()
tctl = Dyre.wrapMain $ Dyre.defaultParams
  { Dyre.projectName = "tctl"
  , Dyre.realMain    = realMain
  , Dyre.configDir   = Just tctldir
  , Dyre.cacheDir    = Just tctldir
  , Dyre.showError   = \_ emsg -> Left (TctDyreError emsg)
  , Dyre.ghcOpts     = ["-threaded", "-package tct-" ++ version] }
  --, Dyre.ghcOpts     = ["-threaded"] }
  where tctldir = getHomeDirectory >>= \home -> return (home </> "tctl")

