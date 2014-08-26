module Tct.Core.TctM
  (
    TctM (..)
  , TctROState (..) 

  , TctStatus (..)
  , askStatus

    -- * lift IO functions
  , toIO
  , async
  , wait
  , timeout
  , raceWith
  , waitBothTimed

  ) where


import           Control.Applicative (Applicative)
import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Monad (liftM)
import           Control.Monad.Error (MonadError)
import           Control.Monad.Reader (liftIO, MonadIO, ask, local, runReaderT, MonadReader, ReaderT)
import qualified System.Time as Time


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

wait :: Async.Async a -> TctM a
wait = liftIO . Async.wait

--waitEither :: Async.Async a -> Async.Async b -> TctM (Either a b)
--waitEither a1 a2 = liftIO $ Async.waitEither a1 a2

--waitBoth :: Async.Async a -> Async.Async b -> TctM (a,b)
--waitBoth a1 a2 = liftIO $ Async.waitBoth a1 a2

--cancel :: Async.Async a -> TctM ()
--cancel = liftIO . Async.cancel

--concurrently :: TctM a -> TctM (Async.Concurrently a)
--concurrently m = Async.Concurrently `liftM` toIO m


waitBothTimed :: Int -> TctM a -> TctM b -> TctM (Maybe a, Maybe b)
waitBothTimed n m1 m2 = do
  io1 <- toIO $ timeout n m1 
  io2 <- toIO $ timeout n m2
  liftIO $ Async.withAsync io1 $ \a1 ->  
    liftIO $ Async.withAsync io2 $ \a2 -> 
    liftIO $ Async.waitBoth a1 a2

raceWith :: (a -> Bool) -> TctM a -> TctM a -> TctM a
raceWith p m1 m2 = do
  io1 <- toIO m1
  io2 <- toIO m2
  liftIO $ raceWithIO p io1 io2

raceWithIO :: (a -> Bool) -> IO a -> IO a -> IO a
raceWithIO p m1 m2 = 
  Async.withAsync m1 $ \a1 ->
  Async.withAsync m2 $ \a2 -> do
    e <- Async.waitEither a1 a2
    case e of
      Left  r1 -> if p r1 then Async.cancel a2 >> return r1 else Async.wait a2
      Right r2 -> if p r2 then Async.cancel a1 >> return r2 else Async.wait a1


timeout :: Int -> TctM a -> TctM (Maybe a)
timeout n m
  | n < 0 = Just `liftM` m
  | n == 0 = return Nothing
  | otherwise = do 
    e <- toIO m' >>= liftIO . Async.race (threadDelay n)
    return $ either (const Nothing) Just e
    where
      m' = do
        Time.TOD sec pico <- liftIO Time.getClockTime
        let newTime = Just $ Time.TOD (sec + toSec (toInteger n)) pico
        local (\ r -> r { stopTime = min newTime (stopTime r) }) m
      toSec i = i*1000000

