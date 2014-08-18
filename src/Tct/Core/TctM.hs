module Tct.Core.TctM

where


import           Control.Applicative (Applicative)
import qualified Control.Concurrent.Async as Async (Async, async, wait)
import           Control.Monad.Error (MonadError)
import           Control.Monad.Reader (liftIO, MonadIO, ask, local, runReaderT, MonadReader, ReaderT)
import qualified System.Time as Time
import qualified System.Timeout as Timeout


data TctROState = TctROState
  { satSolverExe :: FilePath
  , smtSolverExe :: FilePath
  , startTime    :: Time.ClockTime
  , stopTime     :: Maybe Time.ClockTime }

newtype TctM r = TctM { runTct :: ReaderT TctROState IO r}
    deriving (Monad, Applicative, MonadIO, MonadReader TctROState, Functor, MonadError IOError)

data TctStatus prob = TctStatus
  { currentProblem :: prob 
  , runningTime    :: Int
  , remainingTime  :: Maybe Int }

askState :: TctM TctROState
askState = ask

askStatus :: prob -> TctM (TctStatus prob)
askStatus prob = do
    st <- askState
    now <- liftIO Time.getClockTime
    return TctStatus 
      { currentProblem = prob
      , runningTime    = Time.tdSec (Time.diffClockTimes now (startTime st))
      , remainingTime  = (Time.tdSec . flip Time.diffClockTimes now) `fmap` stopTime st }

toIO :: TctM a -> TctM (IO a)
toIO m = runReaderT (runTct m) `fmap` askState

async :: TctM a -> TctM (Async.Async a)
async m = toIO m >>= liftIO . Async.async

-- TODO: check if works with calling minisat or so; does not work with foreign function calls
timeout :: Int -> TctM a -> TctM (Maybe a)
timeout n m = toIO m' >>= liftIO . Timeout.timeout n'
  where 
    m' = do 
      Time.TOD sec pico <- liftIO Time.getClockTime 
      let newTime = Just (Time.TOD (sec + toInteger n') pico)
      local (\ r -> r { stopTime = min newTime (stopTime r) }) m
    n' = n * 1000000

wait :: Async.Async a -> TctM a
wait = liftIO . Async.wait

