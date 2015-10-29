module Tct.Core.Common.Concurrent
  ( spawn
  , spawn'
  ) where


import           Control.Concurrent      (forkIO)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Control.Exception       as C
import           Control.Monad           (void)
import           System.Exit             (ExitCode (..))
import           System.IO
import           System.IO.Error         (tryIOError)
import           System.Process


rnf :: String -> ()
rnf = foldr seq ()

-- | Replaces 'System.Process.readProcessWithExitCode' as this sometimes causes "waitForProcess: does not exist (No
-- child processes)" in our setting. Even when the computation is successfull.
-- So we try to catch it here.
--
-- Returns Left error or Right ouput.
spawn :: String -> [String] -> (Handle -> IO ()) -> IO (Either String String)
spawn cmd args putInh = C.bracketOnError create release run where
  create = do
    (Just inh, Just outh, Just errh, ph)
      <- createProcess (proc cmd args)
        { std_in  = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe }
    return (inh, outh, errh, ph)

  release (inh, outh, errh, ph) = do
    terminateProcess ph
    _ <- tryIOError $ hClose inh
    hClose outh >> hClose errh
    _ <- forkIO $ C.catch (void $ waitForProcess ph) (\e -> void (return (e :: C.SomeException)))
    return ()

  run (inh, outh, errh, ph) = do
    outs <- hGetContents outh
    errs <- hGetContents errh
    outMVar <- newEmptyMVar
    _ <- forkIO $ C.evaluate (rnf outs) >> putMVar outMVar ()
    putInh inh
    hClose inh
    takeMVar outMVar
    ex <- waitForProcess ph
    return $ case ex of
      ExitFailure i -> Left ("Tct.Core.Common.Concurrent.spawn: Exitcode: " ++ show i ++ ": " ++ errs)
      ExitSuccess   -> Right outs

-- | Like 'spawn' but does not obtain any input.
spawn' :: String -> [String] -> IO (Either String String)
spawn' cmd args = C.bracketOnError create release run where
  create = do
    (_, Just outh, Just errh, ph)
      <- createProcess (proc cmd args)
        { std_out = CreatePipe
        , std_err = CreatePipe }
    return (outh, errh, ph)

  release (outh, errh, ph) = do
    terminateProcess ph
    hClose outh
    hClose errh
    _ <- forkIO $ C.catch (void $ waitForProcess ph) (\e -> void (return (e :: C.SomeException)))
    return ()

  run (outh, errh, ph) = do
    outs <- hGetContents outh
    errs <- hGetContents errh
    outMVar <- newEmptyMVar
    _ <- forkIO $ C.evaluate (rnf outs) >> putMVar outMVar ()
    takeMVar outMVar
    ex <- waitForProcess ph
    return $ case ex of
      ExitFailure i -> Left ("Tct.Core.Common.Concurrent.spawn: Exitcode: " ++ show i ++ ": " ++ errs)
      ExitSuccess   -> Right outs

