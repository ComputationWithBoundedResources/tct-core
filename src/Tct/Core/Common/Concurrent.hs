module Tct.Core.Common.Concurrent where


import           Control.Concurrent      (forkIO)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Control.Exception       as C
import           Control.Monad           (void)
import           System.Exit             (ExitCode (..))
import           System.IO
import           System.IO.Error         (tryIOError)
import           System.Process


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
    _ <- tryIOError $ forkIO (void $ waitForProcess ph)
    return ()

  run (inh, outh, errh, ph) = do
    outs <- hGetContents outh
    errs <- hGetContents errh
    outMVar <- newEmptyMVar
    _ <- forkIO $ C.evaluate (length outs) >> putMVar outMVar ()
    putInh inh
    hClose inh
    takeMVar outMVar
    ex <- waitForProcess ph
    return $ case ex of
      ExitFailure i -> Left ("Exitcode:" ++ show i ++ ". " ++ errs)
      ExitSuccess -> Right outs

