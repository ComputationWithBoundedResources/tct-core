-- | This module provides the main function and the command-line interface.
module Tct.Core.Main
  (
  version
  -- * Tct Configuration
  , TctConfig (..)
  , defaultTctConfig
  -- * Tct Initialisation
  , apply
  , applyMode
  , module Tct.Core.Main.Options
  , module Tct.Core.Main.Mode
  ) where


import qualified Config.Dyre                as Dyre (Params (..), defaultParams, wrapMain)
import           Control.Applicative        ((<$>), (<*>))
import           Control.Monad.Reader       (runReaderT)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (mconcat)
import qualified Options.Applicative        as O
import           System.Directory           (getHomeDirectory)
import           System.Exit                (exitFailure, exitSuccess)
import           System.FilePath            ((</>))
import           System.IO                  (hPrint, stderr)
import qualified System.Time                as Time

import           Tct.Core.Combinators       (declarations)
import           Tct.Core.Common.Error
import qualified Tct.Core.Common.Pretty     as PP
import           Tct.Core.Data
import           Tct.Core.Main.Mode
import           Tct.Core.Main.Options
import           Tct.Core.Processor.Timeout (timeoutIn)


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


-- TctConfig ---------------------------------------------------------------------------------------------------------

-- | The Tct configuration defines global properties. 
--   It is updated by command-line arguments and 'TctMode'.
data TctConfig prob = TctConfig
  { outputMode :: OutputMode
  , strategies :: [StrategyDeclaration prob] }

-- | Output mode.
data OutputMode
  = OnlyAnswer
  | WithProof
  | WithXml

instance Show OutputMode where
  show OnlyAnswer = "a"
  show WithProof  = "p"
  show WithXml    = "x"

readOutputMode :: Monad m => String -> m OutputMode
readOutputMode s
  | s == show OnlyAnswer = return  OnlyAnswer
  | s == show WithProof  = return  WithProof
  | s == show WithXml    = return  WithXml
  | otherwise = fail $ "Tct.readOutputMode: " ++ s

-- | The default Tct configuration. A good starting point for custom configurations.
defaultTctConfig :: ProofData prob => TctConfig prob
defaultTctConfig = TctConfig
  { outputMode = OnlyAnswer
  , strategies = declarations }

type TctConfiguration prob opt = Either TctError (TctConfig prob, TctMode prob opt)

tctl :: ProofData prob => TctConfiguration prob opt -> IO ()
tctl = Dyre.wrapMain $ Dyre.defaultParams
  { Dyre.projectName = "tctl"
  , Dyre.realMain    = realMain
  , Dyre.configDir   = Just tctldir
  , Dyre.cacheDir    = Just tctldir
  , Dyre.showError   = \_ emsg -> Left (TctDyreError emsg)
  , Dyre.ghcOpts     = ["-threaded", "-package tct-core-" ++ version] }
  where tctldir = getHomeDirectory >>= \home -> return (home </> "tctl")


-- Mode Application --------------------------------------------------------------------------------------------------

-- | Construct a customised Tct. Example usage:
--
-- > main = tctl $ apply defaultTctConfig trsMode
apply :: ProofData prob => TctConfig prob -> TctMode prob opt -> IO ()
apply c m = tctl $ Right (c,m)

-- | Construct a customised Tct with default configuration.
--
-- > applyMode m = apply defaultTctConfig m
applyMode :: ProofData prob => TctMode prob opt -> IO ()
applyMode = apply defaultTctConfig


-- Command-Line Options ----------------------------------------------------------------------------------------------

-- | Tct command line options.
data TctOptions m = TctOptions
  { outputMode_   :: Maybe OutputMode
  , timeout_      :: Maybe Int
  , modeOptions_  :: m
  , strategyName_ :: Maybe String
  , problemFile_  :: FilePath }

updateTctConfig :: TctConfig prob -> TctOptions m -> TctConfig prob
updateTctConfig cfg opt = cfg { outputMode = outputMode cfg `fromMaybe` outputMode_ opt }

mkParser :: [StrategyDeclaration proc] -> O.Parser m -> O.ParserInfo (TctOptions m)
mkParser ps mparser = O.info (versioned <*> listed <*> O.helper <*> tctp) desc
  where
    listed = O.infoOption (PP.display . PP.vcat $ map PP.pretty ps) $ mconcat
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
        , O.short 'a'
        , O.long "answer"
        , O.helpDoc . Just $ PP.vcat
          [ PP.hsep [PP.text (show OnlyAnswer), PP.char '-', PP.text "only answer"]
          , PP.hsep [PP.text (show WithProof) , PP.char '-', PP.text "with proof"]
          , PP.hsep [PP.text (show WithXml)   , PP.char '-', PP.text "with xml"] ]]))
      <*> O.optional (O.option (mconcat
        [ O.short 't'
        , O.long "timeout"
        , O.metavar "Sec"
        , O.help "Sets timeout in seconds."]))
      <*> mparser
      <*> O.optional (O.strOption (mconcat
        [O.long "strategy"
        , O.short 's'
        , O.help "The strategy to apply."]))
      <*> O.argument O.str (O.metavar "File")
    desc = mconcat
      [ O.headerDoc   . Just $ PP.string "TcT -- Tyrolean Complexity Tool"
      , O.progDescDoc . Just $ PP.string owl ]


-- Main --------------------------------------------------------------------------------------------------------------

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
        , timeout_      = theTimeout
        , problemFile_  = theProblemFile
        , modeOptions_  = theOptions
        } = opts
      ucfg = updateTctConfig cfg opts
      TctConfig
        { outputMode = theOutputMode } = ucfg
    prob <- mkProblem theProblemFile theProblemParser theModifyer theOptions
    st   <- mkStrategy theDefaultStrategy theStrategies theStrategyName
    let stt = maybe st (`timeoutIn` st) theTimeout
    r    <- runIt cfg stt prob
    output theOutputMode theAnswer r
  case r of
    Left err -> hPrint stderr err >> exitFailure
    Right _  -> exitSuccess

  where
    mkOptions optParser strats = liftIO $ O.execParser (mkParser strats optParser)
    mkStrategy :: Strategy prob -> [StrategyDeclaration prob] -> Maybe String -> ErrorT TctError IO (Strategy prob)
    mkStrategy def strats = maybe
      (return def)
      (liftEither . parseStrategy strats)
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
        --WithXml   -> Xml.putXml $ Xml.toXml pt
        _         -> return ()
    putPretty :: PP.Pretty a => a -> IO ()
    putPretty = putStrLn . PP.display . PP.pretty
    parseStrategy sds s = case strategyFromString sds s of
      Left err -> Left $ TctParseError (show err)
      Right st -> Right st

