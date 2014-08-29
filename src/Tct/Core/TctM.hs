-- | This module provides the Tct monad.
module Tct.Core.TctM
  (
  -- * Tct Monad
  TctM (..)
  , TctROState (..)
  , TctStatus (..)
  , askStatus

  -- * Lifted IO functions
  , async
  , wait
  , timeout
  , raceWith
  , concurrently
  ) where


import           Control.Applicative      (Applicative, (<$>), (<*>))
import           Control.Concurrent       (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Monad            (liftM)
import           Control.Monad.Error      (MonadError)
import           Control.Monad.Reader     (MonadIO, MonadReader, ReaderT, ask, liftIO, local, runReaderT)
import qualified System.Time              as Time


-- | Provides Tct runtime options.
data TctROState = TctROState
  { satSolverExe :: FilePath
  , smtSolverExe :: FilePath
  , startTime    :: Time.ClockTime
  , stopTime     :: Maybe Time.ClockTime }

-- | The Tct monad.
newtype TctM r = TctM { runTct :: ReaderT TctROState IO r}
    deriving (Monad, Applicative, MonadIO, MonadReader TctROState, Functor, MonadError IOError)

-- | Defines (dynamic) runtime status.
data TctStatus prob = TctStatus
  { currentProblem :: prob
  , runningTime    :: Int
  , remainingTime  :: Maybe Int }

askState :: TctM TctROState
askState = ask

-- | Returns 'TctStatus' which is derived from 'TctROState' during runtime.
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

-- | Lifts 'Async.async'.
async :: TctM a -> TctM (Async.Async a)
async m = toIO m >>= liftIO . Async.async

-- | Lifts 'Async.wait'.
wait :: Async.Async a -> TctM a
wait = liftIO . Async.wait

--waitEither :: Async.Async a -> Async.Async b -> TctM (Either a b)
--waitEither a1 a2 = liftIO $ Async.waitEither a1 a2

-- | Lifts 'Async.concurrently'.
concurrently :: TctM a -> TctM b -> TctM (a,b)
concurrently m1 m2 = do
  (io1,io2) <- (,) <$> toIO m1 <*> toIO m2
  liftIO $ Async.withAsync io1 $ \a1 ->
    liftIO $ Async.withAsync io2 $ \a2 ->
    liftIO $ Async.waitBoth a1 a2

-- | @'raceWith' p1 p2 m1 m2@ runs @m1@ and @m2@ in parallel.
--
-- * Returns the first result that fulfills @p1@.
-- * If none fullfills @p1@, it returns the first result that fulfills @p2@.
-- * If none fullfills neiter @p1@ nor @p2@, it returns the latter result.
raceWith :: (a -> Bool) -> (a -> Bool) -> TctM a -> TctM a -> TctM a
raceWith p1 p2 m1 m2 = do
  (io1,io2) <- (,) <$> toIO m1 <*> toIO m2
  liftIO $ raceWithIO p1 p2 io1 io2

-- TODO refactor
raceWithIO :: (a -> Bool) -> (a -> Bool)-> IO a -> IO a -> IO a
raceWithIO p1 p2 m1 m2 =
  Async.withAsync m1 $ \a1 ->
  Async.withAsync m2 $ \a2 -> do
    e <- Async.waitEither a1 a2
    case e of
      Left  r1
        | p1 r1     -> Async.cancel a2 >> return r1
        | p2 r1     -> Async.wait a2 >>= \r2 -> return (if not (p2 r2) then r1 else r2)
        | otherwise -> Async.wait a2
      Right r2
        | p1 r2     -> Async.cancel a1 >> return r2
        | p2 r2     -> Async.wait a1 >>= \r1 -> return (if not (p2 r2) then r2 else r1)
        | otherwise -> Async.wait a1

-- | @'timeout' i m@ wraps the Tct action into a timeout, and locally sets 'stopTime'.
-- If @i@ is negative @m@ may run forever.
-- Returns 'Nothing'  if @m@ does not end before the timeout.
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

