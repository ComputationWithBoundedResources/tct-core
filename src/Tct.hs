{- | This module provides the user interaction.

-}

module Tct
  (
    version
  -- * TctMode
  , TctMode (..)
  , apply
  , applyMode
  , void
  -- * TctConfig
  , TctConfig (..)
  )
  where


import qualified Config.Dyre          as Dyre (Params (..), defaultParams, wrapMain)
import           Control.Applicative  (pure, (<$>), (<*>))
import           Control.Monad        (liftM)
import           Control.Monad.Reader (runReaderT)
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          (mconcat)
import qualified Options.Applicative  as O
import           System.Directory     (getHomeDirectory)
import           System.Exit          (exitFailure, exitSuccess)
import           System.FilePath      ((</>))
import           System.IO            (hPrint, stderr)
import qualified System.Time          as Time

import           Tct.Combinators
import           Tct.Common.Error
import qualified Tct.Common.Pretty    as PP
import qualified Tct.Common.Xml       as Xml
import           Tct.Core


-- | Current version.
version :: String
version = "3.0.0"

owl :: String
owl = unlines
  [ " ,___, "
  , " [O.o]   - TcT is a transformer framework for automated complexity analysis."
  , "/)___) "
  , "--\"-\"-"
  ]


-- * TctMode ---------------------------------------------------------------------------------------------------------

-- | 'TctMode' provides all infromation necesary to construct a Tct customised for a problem type.
data TctMode prob opt = TctMode
  { modeParser          :: String -> Either TctError prob    -- ^ The parser for the problem.
  , modeStrategies      :: [SomeParsableProcessor prob]      -- ^ Problem specific parsable Processor/Strategies.
                                                             --   These are added to default 'processors'.
  , modeDefaultStrategy :: Strategy prob                     -- ^ The default strategy to execute.
  , modeOptions         :: O.Parser opt                      -- ^ Problem specific option parser.
                                                             --   These are added to the standard Tct options.
  , modeModifyer        :: prob -> opt -> prob               -- ^ This function is applied to the initial problem,
                                                             --   using the options parsed from command line.
  , modeAnswer          :: Return (ProofTree prob) -> Answer -- ^ Custom Answer.
  }

-- | Construct customised Tct mode. Example usage.
--
-- > main = tctl $ apply defaultTctConfig void
apply :: ProofData prob => TctConfig prob -> TctMode prob opt -> IO ()
apply c m = tctl $ Right (c,m)

-- | Construct a customised Tct with default configuration.
--
-- > applyMode m = apply defaultTctConfig m
applyMode :: ProofData prob => TctMode prob opt -> IO ()
applyMode = apply defaultTctConfig

data Void = Void deriving (Show, Read)
instance PP.Pretty Void where pretty = const $ PP.string "Void"

instance Xml.Xml Void where
  toXml _ = Xml.elt "void" []

-- | An example 'TctMode'.
void :: TctMode Void Void
void = TctMode
  { modeParser          = const (Right Void)
  , modeStrategies      = []
  , modeDefaultStrategy = Proc abort
  , modeOptions         = pure Void
  , modeModifyer        = const id
  , modeAnswer          = const (answer Void)}


-- * TctConfig -------------------------------------------------------------------------------------------------------

-- | Output mode.
data OutputMode
  = OnlyAnswer
  | WithProof
  | WithXml

readOutputMode :: Monad m => String -> m OutputMode
readOutputMode s
  | s == "a" = return  OnlyAnswer
  | s == "p" = return  WithProof
  | s == "x" = return  WithXml
  | otherwise = fail $ "Tct.readOutputMode: " ++ s

-- | Tct configuration.
-- Defines global properties. Is updated by command line arguments and 'TctMode'.
data TctConfig prob = TctConfig
  { outputMode :: OutputMode
  , strategies :: [SomeParsableProcessor prob] }

-- | The default Tct configuration.
defaultTctConfig :: ProofData prob => TctConfig prob
defaultTctConfig = TctConfig
  { outputMode = OnlyAnswer
  , strategies = parsableProcessors }

-- | Tct command line options.
data TctOptions m = TctOptions
  { outputMode_   :: Maybe OutputMode
  , modeOptions_  :: m
  , strategyName_ :: Maybe String
  , problemFile_  :: FilePath }

updateTctConfig :: TctConfig prob -> TctOptions m -> TctConfig prob
updateTctConfig cfg opt = cfg { outputMode = outputMode cfg `fromMaybe` outputMode_ opt }

mkParser :: [SomeParsableProcessor proc] -> O.Parser m -> O.ParserInfo (TctOptions m)
mkParser ps mparser = O.info (versioned <*> listed <*> O.helper <*> tctp) desc
  where
    listed = O.infoOption (PP.display $ mkDescription ps) $ mconcat
      [ O.long "list"
      , O.help "Display list of strategies."]
    versioned = O.infoOption version  $ mconcat
      [ O.long "version"
      , O.short 'v'
      , O.help "Display Version."
      , O.hidden]
    tctp = TctOptions
      <$> O.optional (O.nullOption (mconcat
        [ O.reader readOutputMode
        , O.long "answer"
        , O.short 'a'
        , O.help "Help"]))
      <*> mparser
      <*> O.optional (O.strOption (mconcat
        [O.long "strategy"
        , O.short 's'
        , O.help "The strategy to apply."]))
      <*> O.argument O.str (O.metavar "File")
    desc = mconcat
      [ O.headerDoc   . Just $ PP.string "TcT -- Tyrolean Complexity Tool"
      , O.progDescDoc . Just $ PP.string owl ]

run :: TctConfig prob -> TctM a -> IO a
run _ m = do
  time <- Time.getClockTime
  let
    state = TctROState
      { startTime    = time
      , stopTime     = Nothing }
  runReaderT (runTct m) state

realMain :: ProofData prob => TctConfiguration prob opt -> IO ()
realMain dcfg = do
  r <- runErroneousIO $ do
    (cfg, mode) <- liftEither dcfg
    let
      TctMode
        { modeParser           = theProblemParser
        , modeDefaultStrategy  = theDefaultStrategy
        , modeOptions          = theOptionParser
        , modeModifyer         = theModifyer
        , modeAnswer           = theAnswer
        } = mode
      theStrategies = strategies cfg ++ modeStrategies mode
    opts <- mkOptions theOptionParser theStrategies
    let
      TctOptions
        { strategyName_ = theStrategyName
        , problemFile_  = theProblemFile
        , modeOptions_  = theOptions
        } = opts
      ucfg = updateTctConfig cfg opts
      TctConfig
        { outputMode = theOutputMode } = ucfg
    prob <- mkProblem theProblemFile theProblemParser theModifyer theOptions
    st   <- mkStrategy theDefaultStrategy theStrategies theStrategyName
    r    <- runIt cfg st prob
    output theOutputMode theAnswer r
  case r of
    Left err -> hPrint stderr err >> exitFailure
    Right _  -> exitSuccess

  where
    mkOptions optParser strats = liftIO $ O.execParser (mkParser strats optParser)
    mkStrategy def strats = maybe
      (return def)
      (liftEither . liftM Proc . parseSomeParsableProcessor strats)
    mkProblem file parser modifyer opts = do
      f <- tryIO $ readFile file
      liftEither $ do
        prob <- parser f
        return $ modifyer prob opts
    runIt cfg st prob = liftIO $ run cfg (evaluate st prob)
    output a f r = liftIO $ do
      putPretty $ f r
      let pt = fromReturn r
      case a of
        WithProof -> putPretty pt
        WithXml   -> Xml.putXml $ Xml.toXml pt
        _         -> return ()
    putPretty :: PP.Pretty a => a -> IO ()
    putPretty = putStrLn . PP.display . PP.pretty


type TctConfiguration prob opt = Either TctError (TctConfig prob, TctMode prob opt)

tctl :: ProofData prob => TctConfiguration prob opt -> IO ()
tctl = Dyre.wrapMain $ Dyre.defaultParams
  { Dyre.projectName = "tctl"
  , Dyre.realMain    = realMain
  , Dyre.configDir   = Just tctldir
  , Dyre.cacheDir    = Just tctldir
  , Dyre.showError   = \_ emsg -> Left (TctDyreError emsg)
  , Dyre.ghcOpts     = ["-threaded", "-package tct-" ++ version] }
  --, Dyre.ghcOpts     = ["-threaded"] }
  where tctldir = getHomeDirectory >>= \home -> return (home </> "tctl")

