-- | This module provides the main function and the command-line interface.
-- We use 'Config.Dyre' to define configurations and extend the declarations list. The configuration directory is
-- currently fixed @~/.tct3@. The 'modeId' defines the configuration file. For example @~/.tct3/trs.hs@
-- The interactive mode also has to be started withing @~/.tct3@.
module Tct.Core.Main
  (
  version
  -- * Tct Configuration
  , TctConfig (..)
  , defaultTctConfig
  , OutputMode (..)
  -- * Tct Initialisation
  , setMode
  , setModeWith
  , run
  , runInteractive
  , module M
  ) where


import qualified Config.Dyre                as Dyre (Params (..), defaultParams, wrapMain)
import           Control.Applicative        ((<$>), (<*>), (<|>))
import           Control.Monad.Reader       (runReaderT)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (mconcat)
import qualified Options.Applicative        as O
import           System.Directory           (getHomeDirectory, setCurrentDirectory)
import           System.Exit                (exitFailure, exitSuccess)
import           System.FilePath            ((</>))
import           System.IO                  (hPrint, stderr)
import           System.IO.Temp             (withTempDirectory)
import           System.Process             (system)
import qualified System.Time                as Time

import           Tct.Core.Data              as M (ProofTree, answer)
import           Tct.Core.Main.Mode         as M
import           Tct.Core.Main.Options      as M

import           Tct.Core.Common.Error
import qualified Tct.Core.Common.Pretty     as PP
import           Tct.Core.Data
import           Tct.Core.Declarations      (declarations)
import           Tct.Core.Parse             (strategyFromString)
import           Tct.Core.Processor.Timeout (timeoutIn)


-- | Current version.
version :: String
version = "3.0.0"

synopsis :: String
synopsis = "TcT is a transformer framework for automated complexity analysis."


-- TctConfig ---------------------------------------------------------------------------------------------------------

-- | The Tct configuration defines global properties.
--   It is updated by command-line arguments and 'TctMode'.
data TctConfig i = TctConfig
  { outputMode :: OutputMode
  , recompile  :: Bool
  , strategies :: [StrategyDeclaration i i] }

-- | Output mode.
data OutputMode
  = OnlyAnswer
  | WithProof
  | WithDetailedProof
  | AsXml
  | CustomAnswer

instance Show OutputMode where
  show OnlyAnswer        = "a"
  show WithProof         = "p"
  show WithDetailedProof = "d"
  show AsXml             = "x"
  show CustomAnswer      = "c"

readOutputMode :: Monad m => String -> m OutputMode
readOutputMode s
  | s == show OnlyAnswer        = return OnlyAnswer
  | s == show WithProof         = return WithProof
  | s == show WithDetailedProof = return WithDetailedProof
  | s == show AsXml             = return AsXml
  | s == show CustomAnswer      = return CustomAnswer
  | otherwise = fail $ "Tct.readOutputMode: " ++ s

-- | The default Tct configuration. A good starting point for custom configurations.
defaultTctConfig :: ProofData i => TctConfig i
defaultTctConfig = TctConfig
  { outputMode = OnlyAnswer
  , recompile  = True
  , strategies = declarations }

configDir :: IO FilePath
configDir = getHomeDirectory >>= \home -> return (home </> ".tct3")

{-configFile :: String -> IO FilePath-}
{-configFile n = configDir >>= return . (</> n)-}

type TctConfiguration i opt = Either TctError (TctConfig i,  TctMode i i opt)

tct3 :: ProofData i => TctConfiguration i opt -> IO ()
tct3 conf = Dyre.wrapMain params conf
  where
    params = Dyre.defaultParams
      { Dyre.projectName = name
      , Dyre.configCheck = either (const True) (recompile . fst) conf
      , Dyre.realMain    = realMain
      , Dyre.configDir   = Just configDir
      , Dyre.cacheDir    = Just configDir
      , Dyre.showError   = \_ emsg -> Left (TctDyreError emsg)
      , Dyre.ghcOpts     = ghcOpts}
      --, Dyre.ghcOpts     = ["-threaded", "-package tct-its-" ++ version] }
    name    = either (const "all") (modeId . snd) conf
    ghcOpts =
      ["-threaded", "-O","-fno-spec-constr-count", "-rtsopts", "-with-rtsopts=-N"]

--- * Mode Application -----------------------------------------------------------------------------------------------

-- | Construct a customised Tct. Example usage:
--
-- > main = tct3 $ setModeWith defaultTctConfig trsMode
setModeWith :: ProofData i => TctConfig i -> TctMode i i opt -> IO ()
setModeWith c m = tct3 $ Right (c,m)

-- | Construct a customised Tct with default configuration.
--
-- > setMode m = setModeWith defaultTctConfig m
setMode :: ProofData i => TctMode i i opt -> IO ()
setMode = setModeWith defaultTctConfig


--- * Command-Line Options -------------------------------------------------------------------------------------------

-- | Tct command line options.
data TctOptions m = TctOptions
  { outputMode_   :: Maybe OutputMode
  , timeout_      :: Maybe Int
  , modeOptions_  :: m
  , strategyName_ :: Maybe String
  , problemFile_  :: FilePath }

updateTctConfig :: TctConfig i -> TctOptions m -> TctConfig i
updateTctConfig cfg opt = cfg { outputMode = outputMode cfg `fromMaybe` outputMode_ opt }


data TctAction m
  = Run (TctOptions m)
  | RunInteractive

mkParser :: [StrategyDeclaration i o] -> O.Parser m -> O.ParserInfo (TctAction m)
mkParser ps mparser = O.info (versioned <*> listed <*> O.helper <*> interactive <|> tctp) desc
  where
    listed = O.infoOption (PP.display . PP.vcat $ map PP.pretty ps) $ mconcat
      [ O.long "list"
      , O.help "Display list of strategies."]
    versioned = O.infoOption version  $ mconcat
      [ O.long "version"
      , O.short 'v'
      , O.help "Display Version."
      , O.hidden]
    interactive = O.flag' RunInteractive  $ mconcat
      [ O.long "interactive"
      , O.short 'i'
      , O.help "Interactive mode (experimental)." ]
    tctp = fmap Run $ TctOptions
      <$> O.optional (O.option (O.str >>= readOutputMode) (mconcat
        [ O.short 'a'
        , O.long "answer"
        , O.helpDoc . Just $ PP.vcat
          [ PP.hsep [PP.text (show OnlyAnswer)        , PP.text "- only answer"]
          , PP.hsep [PP.text (show WithProof)         , PP.text "- with proof"]
          , PP.hsep [PP.text (show WithDetailedProof) , PP.text "- with detailed proof"]
          , PP.hsep [PP.text (show AsXml)             , PP.text "- as xml"]
          , PP.hsep [PP.text (show CustomAnswer)      , PP.text "- as custom answer"] ]]))
      <*> O.optional (O.option O.auto (mconcat
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
      , O.progDescDoc . Just $ PP.string synopsis ]


--- * Main -----------------------------------------------------------------------------------------------------------

run :: TctM a -> IO a
run m = do
  time <- Time.getClockTime
  let
    state tmp = TctROState
      { startTime     = time
      , stopTime      = Nothing
      , tempDirectory = tmp }
  withTempDirectory "/tmp" "tctx" (runReaderT (runTct m) . state)

runInteractive :: String -> IO ()
runInteractive theModeId = do
  ret <- runErroneousIO $ do
    _ <- tryIO $ setCurrentDirectory `fmap` configDir
    _ <- tryIO $ system $ "ghci +RTS -N -RTS -package tct-" ++ theModeId ++ " " ++ theModeId ++ ".hs"
    return ()
  either print return ret


realMain :: ProofData i => TctConfiguration i opt -> IO ()
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
    action <- mkOptions theOptionParser theStrategies
    case action of
      RunInteractive -> tryIO $ runInteractive (modeId mode)
      Run opts       -> do
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

        prob <- do
          f <- tryIO $ readFile theProblemFile
          liftEither $ theModifyer theOptions `fmap` theProblemParser f

        st   <- maybe (return theDefaultStrategy) (liftEither . parseStrategy theStrategies) theStrategyName

        let stt = maybe st (`timeoutIn` st) theTimeout
        r    <- liftIO $ run (evaluate stt prob)
        output theOutputMode (theAnswer theOptions) r
  case r of
    Left err -> hPrint stderr err >> exitFailure
    Right _  -> exitSuccess

  where
    mkOptions optParser strats = liftIO $ O.execParser (mkParser strats optParser)

    output v custom ret = liftIO $
      case (v,ret) of
        (CustomAnswer, _)      -> custom ret

        (_, Halt pt)           -> PP.putPretty MaybeAnswer >> PP.putPretty (ppProofTree PP.pretty pt)
        (OnlyAnswer, r)        -> PP.putPretty (answer $ fromReturn r)
        (WithProof, r)         -> PP.putPretty (answer $ fromReturn r) >> PP.putPretty (ppProofTree PP.pretty $ fromReturn r)
        (WithDetailedProof, r) -> PP.putPretty (answer $ fromReturn r) >> PP.putPretty (ppDetailedProofTree PP.pretty $ fromReturn r)
        (AsXml, _)             -> error "missing: toXml prooftree" -- TODO:
    parseStrategy sds s = case strategyFromString sds s of
      Left err -> Left $ TctParseError (show err)
      Right st -> Right st

