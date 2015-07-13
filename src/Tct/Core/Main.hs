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
  , defaultTctInteractiveConfig
  , AnswerFormat (..)
  , ProofFormat (..)
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

import           Tct.Core.Data              as M (ProofTree, competitionAnswer)
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
  { answerFormat  :: AnswerFormat
  , proofFormat   :: ProofFormat
  , recompile     :: Bool
  , strategies    :: [StrategyDeclaration i i]
  , defaultSolver :: Maybe (FilePath, [String]) }

-- | Format of answer output.
data AnswerFormat
  = SilentAnswerFormat
  | DefaultAnswerFormat
  | CompetitionAnswerFormat
  | CustomAnswerFormat

-- | Format of proof output. Printed after answer in main.
data ProofFormat
  = SilentProofFormat
  | DefaultProofFormat
  | VerboseProofFormat
  | XmlProofFormat
  | CustomProofFormat

writeAnswerFormat :: AnswerFormat -> String
writeAnswerFormat SilentAnswerFormat      = "s"
writeAnswerFormat DefaultAnswerFormat     = "d"
writeAnswerFormat CompetitionAnswerFormat = "c"
writeAnswerFormat CustomAnswerFormat      = "x"

readAnswerFormat :: Monad m => String -> m AnswerFormat
readAnswerFormat s
  | s == writeAnswerFormat SilentAnswerFormat      = return SilentAnswerFormat
  | s == writeAnswerFormat DefaultAnswerFormat     = return DefaultAnswerFormat
  | s == writeAnswerFormat CompetitionAnswerFormat = return CompetitionAnswerFormat
  | s == writeAnswerFormat CompetitionAnswerFormat = return CompetitionAnswerFormat
  | otherwise = fail $ "Tct.readOutputMode: " ++ s

writeProofFormat :: ProofFormat -> String
writeProofFormat SilentProofFormat  = "s"
writeProofFormat DefaultProofFormat = "d"
writeProofFormat VerboseProofFormat = "v"
writeProofFormat XmlProofFormat     = "x"
writeProofFormat CustomProofFormat  = "c"

readProofFormat :: Monad m => String -> m ProofFormat
readProofFormat s
  | s == writeProofFormat SilentProofFormat  = return SilentProofFormat
  | s == writeProofFormat DefaultProofFormat = return DefaultProofFormat
  | s == writeProofFormat VerboseProofFormat = return VerboseProofFormat
  | s == writeProofFormat XmlProofFormat     = return XmlProofFormat
  | s == writeProofFormat CustomProofFormat  = return CustomProofFormat
  | otherwise = fail $ "Tct.readOutputMode: " ++ s


defaultTctConfig' :: TctConfig i
defaultTctConfig' = TctConfig
  { answerFormat  = DefaultAnswerFormat
  , proofFormat   = DefaultProofFormat
  , recompile     = True
  , strategies    = []
  , defaultSolver = Nothing }

-- | The default Tct configuration. A good starting point for custom configurations.
defaultTctConfig :: ProofData i => TctConfig i
defaultTctConfig = defaultTctConfig' { strategies = declarations }

-- MS: in the interactive mode we can not ensure ProofData i; for some i
-- | The default Tct configuration for the interactive mode.
defaultTctInteractiveConfig :: TctConfig i
defaultTctInteractiveConfig = defaultTctConfig'

configDir :: IO FilePath
configDir = getHomeDirectory >>= \home -> return (home </> ".tct3")

{-configFile :: String -> IO FilePath-}
{-configFile n = configDir >>= return . (</> n)-}

type TctConfiguration i opt = Either TctError (TctConfig i,  TctMode i i opt)

tct3 :: ProofData i => TctConfiguration i opt -> IO ()
tct3 conf = Dyre.wrapMain params conf
  where
    params = Dyre.defaultParams
      { Dyre.projectName = "tct-" ++ name
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
-- > main = tct3 $ trsMode `setModeWith` defaultTctConfig
setModeWith :: ProofData i => TctMode i i opt -> TctConfig i -> IO ()
setModeWith m c = tct3 $ Right (c,m)

-- | Construct a customised Tct with default configuration.
--
-- > setMode m = m `setModeWith` defaultTctConfi
setMode :: ProofData i => TctMode i i opt -> IO ()
setMode = flip setModeWith defaultTctConfig


--- * Command-Line Options -------------------------------------------------------------------------------------------

-- | Tct command line options.
data TctOptions m = TctOptions
  { answerFormat_ :: Maybe AnswerFormat
  , proofFormat_  :: Maybe ProofFormat
  , timeout_      :: Maybe Int
  , modeOptions_  :: m
  , strategyName_ :: Maybe String
  , problemFile_  :: FilePath }

updateTctConfig :: TctConfig i -> TctOptions m -> TctConfig i
updateTctConfig cfg opt = cfg
  { answerFormat = answerFormat cfg `fromMaybe` answerFormat_ opt
  , proofFormat  = proofFormat cfg `fromMaybe` proofFormat_ opt }


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
      <$> O.optional (O.option (O.str >>= readAnswerFormat) (mconcat
        [ O.short 'a'
        , O.long "answer"
        , O.helpDoc . Just $ PP.vcat
          [ PP.hsep [PP.text (writeAnswerFormat SilentAnswerFormat)     , PP.text "- silent"]
          , PP.hsep [PP.text (writeAnswerFormat DefaultAnswerFormat)    , PP.text "- default answer"]
          , PP.hsep [PP.text (writeAnswerFormat CompetitionAnswerFormat), PP.text "- competition answer"]
          , PP.hsep [PP.text (writeAnswerFormat CustomAnswerFormat)     , PP.text "- custom answer"] ]]))
      <*> O.optional (O.option (O.str >>= readProofFormat) (mconcat
        [ O.short 'p'
        , O.long "proof"
        , O.helpDoc . Just $ PP.vcat
          [ PP.hsep [PP.text (writeProofFormat SilentProofFormat) , PP.text "- silent"]
          , PP.hsep [PP.text (writeProofFormat DefaultProofFormat), PP.text "- default proof"]
          , PP.hsep [PP.text (writeProofFormat VerboseProofFormat), PP.text "- verbose proof"]
          , PP.hsep [PP.text (writeProofFormat XmlProofFormat)    , PP.text "- xml proof"]
          , PP.hsep [PP.text (writeProofFormat CustomProofFormat) , PP.text "- custom proof"] ]]))
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

run :: TctConfig i -> TctM a -> IO a
run cfg m = do
  time <- Time.getClockTime
  let
    state tmp = TctROState
      { startTime     = time
      , stopTime      = Nothing
      , tempDirectory = tmp
      , solver        = defaultSolver cfg }
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
        , modeModifyer         = theModifyer
        , modeDefaultStrategy  = theDefaultStrategy
        , modeOptions          = theOptionParser
        , modeAnswer           = theAnswer
        , modeProof            = theProof
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
            { answerFormat = theAnswerFormat
            , proofFormat  = theProofFormat } = ucfg

        prob <- do
          f <- tryIO $ theProblemParser theProblemFile
          liftEither $ either (Left . TctParseError) Right $ theModifyer theOptions `fmap` f

        st   <- maybe (return theDefaultStrategy) (liftEither . parseStrategy theStrategies) theStrategyName

        let stt = maybe st (`timeoutIn` st) theTimeout
        r    <- liftIO $ run ucfg (evaluate stt prob)
        putAnswer theAnswerFormat (theAnswer theOptions) r
        putProof  theProofFormat  (theProof theOptions) r
  case r of
    Left err -> PP.putPretty MaybeDefaultAnswer >> hPrint stderr err >> exitFailure
    Right _  -> exitSuccess

  where
    mkOptions optParser strats = liftIO $ O.execParser (mkParser strats optParser)

    putAnswer v custom ret = liftIO $
      case (v,ret) of
        (SilentAnswerFormat, _)      -> return ()
        (CustomAnswerFormat, _)      -> custom ret

        (_, Halt _)                  -> PP.putPretty MaybeDefaultAnswer
        (DefaultAnswerFormat, r)     -> PP.putPretty (defaultAnswer     $ fromReturn r)
        (CompetitionAnswerFormat, r) -> PP.putPretty (competitionAnswer $ fromReturn r)
    putProof v custom ret = liftIO $
      case (v,ret) of
        (SilentProofFormat, _)      -> return ()
        (CustomProofFormat, _)      -> custom ret

        (_, Halt pt)                -> PP.putPretty (ppDetailedProofTree PP.pretty pt)
        (DefaultProofFormat, r)     -> PP.putPretty (ppProofTree PP.pretty $ fromReturn r)
        (VerboseProofFormat, r)     -> PP.putPretty (ppDetailedProofTree PP.pretty $ fromReturn r)
        (XmlProofFormat, _)         -> error "missing: toXml proofTree" --TODO

    parseStrategy sds s = case strategyFromString sds s of
      Left err -> Left $ TctParseError (show err)
      Right st -> Right st

