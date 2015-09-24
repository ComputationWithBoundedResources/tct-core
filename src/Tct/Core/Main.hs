-- | This module provides the main function and the command-line interface.
module Tct.Core.Main
  (
  module M
  -- * Tct Configuration
  -- |
  , TctConfig (..)
  , defaultTctConfig
  , InteractiveGHCi (..)
  -- * Tct Initialisation
  , run
  , runInteractive
  , tct3
  , tct3WithOptions
  -- * Pretty Print
  , AnswerFormat (..)
  , ProofFormat (..)
  ) where


import qualified Data.Map as M
import           Control.Applicative        ((<$>), (<*>), (<|>))
import           Control.Monad              (void)
import           Control.Monad.Reader       (runReaderT)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (mconcat)
import qualified Options.Applicative        as O
import           System.Exit                (exitFailure, exitSuccess)
import           System.IO                  (hPrint, stderr, hPutStrLn, hClose)
import           System.IO.Temp             (withTempDirectory, withSystemTempFile)
import           System.Process             (system)
import qualified System.Time                as Time

import           Tct.Core.Data              as M (ProofTree)
import           Tct.Core.Main.Options      as M

import           Tct.Core.Common.Error
import qualified Tct.Core.Common.Pretty     as PP
import           Tct.Core.Data
import           Tct.Core.Declarations      (declarations)
import           Tct.Core.Parse             (strategyFromString)
import           Tct.Core.Processor.Failing (failing)
import           Tct.Core.Processor.Timeout (timeoutIn)


synopsis :: String
synopsis = "TcT is a transformer framework for automated complexity analysis."


-- TctConfig ---------------------------------------------------------------------------------------------------------

-- | The Tct configuration defines global properties.
-- The configuration affects the execution of ('tct3') and sets initial properties ('run') when eva
data TctConfig i = TctConfig
  { parseProblem    :: FilePath -> IO (Either String i)
  , putAnswer       :: Return (ProofTree i) -> IO ()
  , putProof        :: Return (ProofTree i) -> IO ()

  , strategies      :: [StrategyDeclaration i i]
  , defaultStrategy :: Strategy i i

  , runtimeOptions  :: [(String, [String])]

  , interactiveGHCi :: InteractiveGHCi
  , version         :: String
  }

-- | Specifies how interactive mode is executed. See 'runInteractive'.
data InteractiveGHCi = GHCiScript [String] | GHCiCommand String

-- | Default configuration. Minimal requirement 'parseProblem'.
defaultTctConfig :: ProofData i => (FilePath -> IO (Either String i)) -> TctConfig i
defaultTctConfig p = TctConfig
  { parseProblem    = p
  , putAnswer       = PP.putPretty . prettyDefaultAnswer
  , putProof        = PP.putPretty . prettyDefaultProof
  , strategies      = declarations
  , defaultStrategy = failing
  , runtimeOptions  = []
  , interactiveGHCi = GHCiScript
      [ ":set prompt \"tct>\""
      , ":module +Tct.Core.Interactive" ]
  , version         = "3.1.0.0"
  }


prettySilentAnswer, prettyDefaultAnswer, prettyTTTacAnswer :: Return (ProofTree i) -> PP.Doc
prettySilentAnswer  _        = PP.empty
prettyDefaultAnswer (Halt _) = PP.pretty (termcomp unbounded)
prettyDefaultAnswer r        = PP.pretty (termcomp . certificate $ fromReturn r)
prettyTTTacAnswer   (Halt _) = PP.pretty (tttac unbounded)
prettyTTTacAnswer   r        = PP.pretty (tttac    . certificate $ fromReturn r)

prettySilentProof, prettyDefaultProof, prettyVerboseProof :: PP.Pretty i => Return (ProofTree i) -> PP.Doc
prettySilentProof _          = PP.empty
prettyDefaultProof (Halt pt) = PP.pretty (ppDetailedProofTree PP.pretty pt)
prettyDefaultProof r         = PP.pretty (ppProofTree PP.pretty $ fromReturn r)
prettyVerboseProof (Halt pt) = PP.pretty (ppDetailedProofTree PP.pretty pt)
prettyVerboseProof r         = PP.pretty (ppDetailedProofTree PP.pretty $ fromReturn r)
-- prettyXmlProof r             = error "missing: toXml proofTree" -- TODO

putAnswerFormat :: AnswerFormat -> Return (ProofTree i) -> IO ()
putAnswerFormat SilentAnswerFormat  = PP.putPretty . prettySilentAnswer
putAnswerFormat DefaultAnswerFormat = PP.putPretty . prettyDefaultAnswer
putAnswerFormat TTTACAnswerFormat   = PP.putPretty . prettyTTTacAnswer

putProofFormat :: PP.Pretty i => ProofFormat -> Return (ProofTree i) -> IO ()
putProofFormat SilentProofFormat  = PP.putPretty . prettySilentProof
putProofFormat DefaultProofFormat = PP.putPretty . prettyDefaultProof
putProofFormat VerboseProofFormat = PP.putPretty . prettyVerboseProof
-- putXmlFormat XmlProofFormat = PP.putPretty prettyXmlProof


-- | Format of answer output.
data AnswerFormat
  = SilentAnswerFormat
  | DefaultAnswerFormat
  | TTTACAnswerFormat

-- | Format of proof output. Printed after answer in main.
data ProofFormat
  = SilentProofFormat
  | DefaultProofFormat
  | VerboseProofFormat
  -- | XmlProofFormat

writeAnswerFormat :: AnswerFormat -> String
writeAnswerFormat SilentAnswerFormat  = "s"
writeAnswerFormat DefaultAnswerFormat = "d"
writeAnswerFormat TTTACAnswerFormat   = "t"

readAnswerFormat :: Monad m => String -> m AnswerFormat
readAnswerFormat s
  | s == writeAnswerFormat SilentAnswerFormat  = return SilentAnswerFormat
  | s == writeAnswerFormat DefaultAnswerFormat = return DefaultAnswerFormat
  | s == writeAnswerFormat TTTACAnswerFormat   = return TTTACAnswerFormat
  | otherwise = fail $ "Tct.readOutputMode: " ++ s

writeProofFormat :: ProofFormat -> String
writeProofFormat SilentProofFormat  = "s"
writeProofFormat DefaultProofFormat = "d"
writeProofFormat VerboseProofFormat = "v"
-- writeProofFormat XmlProofFormat     = "x"

readProofFormat :: Monad m => String -> m ProofFormat
readProofFormat s
  | s == writeProofFormat SilentProofFormat  = return SilentProofFormat
  | s == writeProofFormat DefaultProofFormat = return DefaultProofFormat
  | s == writeProofFormat VerboseProofFormat = return VerboseProofFormat
  -- | s == writeProofFormat XmlProofFormat     = return XmlProofFormat
  | otherwise = fail $ "Tct.readOutputMode: " ++ s


--- * Command-Line Options -------------------------------------------------------------------------------------------

-- | Tct command line options.
data TctOptions m = TctOptions
  { answerFormat_ :: Maybe AnswerFormat
  , proofFormat_  :: Maybe ProofFormat
  , timeout_      :: Maybe Int
  , modeOptions_  :: m
  , strategyName_ :: Maybe String
  , problemFile_  :: FilePath }


data TctAction m
  = Run (TctOptions m)
  | RunInteractive

mkParser :: [StrategyDeclaration i o] -> String -> O.Parser m -> O.ParserInfo (TctAction m)
mkParser ps vers mparser = O.info (versioned <*> listed <*> O.helper <*> interactive <|> tctp) desc
  where
    listed = O.infoOption (PP.display . PP.vcat $ map PP.pretty ps) $ mconcat
      [ O.long "list"
      , O.help "Display list of strategies."]
    versioned = O.infoOption vers  $ mconcat
      [ O.long "version"
      , O.short 'v'
      , O.help "Display Version."
      , O.hidden]
    interactive = O.flag' RunInteractive $ mconcat
      [ O.long "interactive"
      , O.short 'i'
      , O.help "Interactive mode (experimental)." ]
    tctp = fmap Run $ TctOptions
      <$> O.optional (O.option (O.str >>= readAnswerFormat) (mconcat
        [ O.short 'a'
        , O.long "answer"
        , O.helpDoc . Just $ PP.vcat
          [ PP.hsep [PP.text (writeAnswerFormat SilentAnswerFormat)  , PP.text "- silent"]
          , PP.hsep [PP.text (writeAnswerFormat DefaultAnswerFormat) , PP.text "- default answer (termcomp 2015)"]
          , PP.hsep [PP.text (writeAnswerFormat TTTACAnswerFormat )  , PP.text "- competition answer"] ]]))
      <*> O.optional (O.option (O.str >>= readProofFormat) (mconcat
        [ O.short 'p'
        , O.long "proof"
        , O.helpDoc . Just $ PP.vcat
          [ PP.hsep [PP.text (writeProofFormat SilentProofFormat) , PP.text "- silent"]
          , PP.hsep [PP.text (writeProofFormat DefaultProofFormat), PP.text "- default proof"]
          , PP.hsep [PP.text (writeProofFormat VerboseProofFormat), PP.text "- verbose proof"] ]]))
          -- , PP.hsep [PP.text (writeProofFormat XmlProofFormat)    , PP.text "- xml proof"] ]]))
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
      , kvPairs       = M.fromList (runtimeOptions cfg) }
  withTempDirectory "/tmp" "tctx" (runReaderT (runTct m) . state)

runInteractive :: InteractiveGHCi -> IO ()
runInteractive ig = void $ case ig of
  GHCiCommand cmd -> system cmd
  GHCiScript  scr -> withSystemTempFile "ghcix" $ \fp hf -> do
    hPutStrLn hf (unlines scr) >> hClose hf
    system $ "ghci -ingore-dot-ghci -ghci-script " ++ fp

-- > tct3 = tct3With const unit
tct3 :: ProofData i => TctConfig i -> IO ()
tct3 = tct3WithOptions const unit

-- | Default main function.
--
-- @tct3WithOptions update options config@
--
-- 1. builds a strategy parser from the strategies defined in @config@
-- 2. parses the command line arguments including arguments defined in @options@
-- 3. updates @config@ using the @update@ and the parsed problem options from @option@
-- 4. updates @config@ from standard arguments; in particular answer output and proof output overrides @config@ if given
-- 5. evaluates strategy with the given timeout
-- 6. output answer and proof as given in (updated) @config@
tct3WithOptions :: ProofData i => (TctConfig i -> opt -> TctConfig i) -> Options opt -> TctConfig i -> IO ()
tct3WithOptions theUpdate theOptions cfg = do
  r <- runErroneousIO $ do
    action <- liftIO $ O.execParser $ mkParser (strategies cfg) (version cfg) theOptions
    case action of
      RunInteractive -> tryIO $ runInteractive (interactiveGHCi cfg)
      Run opts       -> do
        let
          TctOptions
            { strategyName_ = theStrategyName
            , timeout_      = theTimeout
            , problemFile_  = theProblemFile
            } = opts
          ucfg = updateTctConfig theUpdate opts cfg
        let
          TctConfig
            { parseProblem    = theParser
            , putAnswer       = theAnswer
            , putProof        = theProof
            , strategies      = theStrategies
            , defaultStrategy = theDefaultStrategy
            } = ucfg

        prob <- do
          f <- tryIO $ theParser theProblemFile
          liftEither $ either (Left . TctParseError) Right f

        st <- maybe (return theDefaultStrategy) (liftEither . parseStrategy theStrategies) theStrategyName

        let stt = maybe st (`timeoutIn` st) theTimeout
        r <- liftIO $ run ucfg (evaluate stt prob)

        liftIO $ theAnswer r
        liftIO $ theProof r
  case r of
    Left err -> PP.putPretty (PP.text "ERROR") >> hPrint stderr err >> exitFailure
    Right _  -> exitSuccess

  where

    updateTctConfig f opt cf = cfg'
      { putAnswer = putAnswer cfg' `fromMaybe` (putAnswerFormat <$> answerFormat_ opt)
      , putProof  = putProof cfg' `fromMaybe` (putProofFormat <$> proofFormat_ opt) }
      where cfg' = f cf (modeOptions_ opt)

    parseStrategy sds s = case strategyFromString sds s of
      Left err -> Left $ TctParseError (show err)
      Right st -> Right st

