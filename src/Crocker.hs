{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Crocker
    ( runCrocker
    , Configuration (..)
    , getConfiguration
    , runConfiguration
    ) where

import           Control.Concurrent             (forkIO, newEmptyMVar, tryPutMVar, takeMVar, forkIO, threadDelay)
import           Control.Concurrent.STM         (atomically, TVar, readTVar, writeTVar, newTVar)
import           Control.Concurrent.STM.TChan   (TChan, newTChan, readTChan, writeTChan)
import           Control.Exception              (catch, throwIO)
import           Control.Monad                  (void, forever, when)
import           Data.Attoparsec.Text           (parse, IResult (..))
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text)
import qualified Data.Text as T
import           Data.Time.Clock                (getCurrentTime, diffUTCTime)
import           Reaper
import           System.Cron
import           System.IO.Error                (isDoesNotExistError)
import           System.Environment             (getArgs)
import           System.Exit                    (ExitCode (..), exitWith)
import           System.Posix.Process           (getAnyProcessStatus, getProcessID, forkProcess, executeFile,
                                                 ProcessStatus (..))
import           System.Posix.Signals           (Handler (Catch), installHandler, sigCHLD, sigINT, sigTERM, sigKILL,
                                                 signalProcess, setStoppedChildFlag, Signal)
import           System.Posix.Types             (ProcessID)

data Configuration = Configuration
    { runAtStart :: Bool
    , schedule   :: CronSchedule
    , command    :: [String]
    } deriving Show

parseArgs :: [String] -> (Bool, [Text])
parseArgs ("--at-start":rest) = (True, T.pack <$> rest)
parseArgs ("-A":rest) = (True, T.pack <$> rest)
parseArgs rest = (False, T.pack <$> rest)

getConfiguration :: Monad m => [String] -> m Configuration
getConfiguration args = do
    let (ras, expr) = parseArgs args
    (cmd, sched) <- parseLoop expr (parse cronScheduleLoose)
    return $ Configuration ras sched (T.unpack <$> cmd)
  where
    parseLoop []     _ = fail "unexpected end of arguments while reading cron expression"
    parseLoop (t:ts) p = case p (t <> " ") of
        Fail x _ err -> fail $ err <> " while reading " <> T.unpack x
        Partial p'   -> parseLoop ts p'
        Done " " r   -> pure (ts, r)
        Done i _     -> fail $ "unrecognised input at end of cron expression: " <> T.unpack i

info :: String -> IO ()
info msg = do
    now <- getCurrentTime
    putStrLn (show now <> ": " <> msg)

runConfiguration :: Configuration -> IO (FilePath, [String])
runConfiguration conf@(Configuration _ sched cmd) = do
    now <- getCurrentTime
    let maxSleep = 2^(20 :: Integer)
    let next = nextMatch sched now
    let delay = round . (* 1000000) . flip diffUTCTime now <$> next :: Maybe Integer
    let sleep = maybe maxSleep (max maxSleep) delay
    threadDelay (fromIntegral sleep)
    upd <- getCurrentTime
    if scheduleMatches sched upd
    then info ("Executing " <> unwords cmd) >> return (head cmd, tail cmd)
    else runConfiguration conf

tt :: IO ()
tt = pid1 $ getConfiguration (words "* * * * * /bin/date") >>= runConfiguration

-- Launch a sub-process
execute :: FilePath -> [String] -> IO ProcessID
execute proc args = forkProcess (executeFile proc True args Nothing)

runCrocker :: IO ()
runCrocker = do
    pid <- getProcessID
    if pid == 1
    then do
        conf <- getArgs >>= getConfiguration
        --TODO when (runAtStart conf) $ callProcess (head $ command conf) (tail $ command conf)
        pid1 $ runConfiguration conf
    else putStrLn "This program must be run as PID 1 inside a docker container."

{- pid1 package isn't general purpose enough for this, so below code is largely taken from there  -}

-- Wait for the specified PID to terminate
waitPid :: ProcessID -> TChan (ProcessID, a) -> IO a
waitPid pid terminated = do
    (ded, code) <- atomically $ readTChan terminated
    if ded == pid then return code else waitPid pid terminated

-- Given an action producing commands to be executed, pid1 will reap all children forever
pid1 :: IO (FilePath, [String]) -> IO ()
pid1 execf = do
    errno <- subreaper
    if errno /= 0
    then fail $ "error " <> show errno
    else pure ()

    -- Shared state
    exiting    <- newEmptyMVar
    terminated <- atomically newTChan
    watching   <- atomically $ newTVar False

    -- Catch SIGTERM and SIGINT to exit all processes
    let startExiting = void . tryPutMVar exiting
    void $ installHandler sigTERM (Catch $ startExiting sigTERM) Nothing
    void $ installHandler sigINT  (Catch $ startExiting sigINT)  Nothing

    -- Catch SIGCHLD, ignoring stopped children
    void $ setStoppedChildFlag False
    void $ installHandler sigCHLD (Catch $ reapWaiting watching terminated `ifNotExistsThen` ()) Nothing

    -- Ask for the next process to be run
    void $ forkIO $ forever $ do
        (fp, args) <- execf
        atomically $ writeTVar watching True
        pid <- execute fp args
        code <- waitPid pid terminated
        atomically $ writeTVar watching False
        case code of
            Exited ExitSuccess     -> info $ "Task completed successfully"
            Exited (ExitFailure c) -> info $ "Task terminated abnormally with exit code " <> show c
            Terminated s d         -> info $ "Task terminated by signal " <> show s <> if d then " (core dumped)" else ""
            Stopped s              -> info $ "Task stopped by signal " <> show s <> " (this should not happen here)"

    -- Then just wait forever for the `exiting` flag to be set.  This should happen on the main thread.
    sig <- takeMVar exiting
    void $ killAllChildren watching terminated sig

ifNotExistsThen :: IO a -> a -> IO a
ifNotExistsThen io a = io `catch` \e ->
    if isDoesNotExistError e
    then pure a
    else throwIO e

-- waitpid() on all terminated child processes; log error exit code if it was a directly spawned child
reapWaiting :: TVar Bool -> TChan (ProcessID, ProcessStatus) -> IO ()
reapWaiting isWatching channel = do
    mstatus <- getAnyProcessStatus blocking noStopped
    case mstatus of
        Nothing     -> pure ()
        Just status -> atomically $ do
            watch <- readTVar isWatching
            when watch $ writeTChan channel status
  where
    blocking = True
    noStopped = False

killAllChildren :: TVar Bool -> TChan (ProcessID, ProcessStatus) -> Signal -> IO ()
killAllChildren watch chan sig = do
    -- fork a process to send the TERM then KILL signals to all processes
    void $ forkIO $ flip ifNotExistsThen () $ do
        info "Sending all processes the TERM signal..."
        signalProcess sigTERM (-1)
        threadDelay $ 5 * 1000 * 1000
        info "Sending all processes the KILL signal..."
        signalProcess sigKILL (-1)

    -- reap all children until there are no more, then terminate
    void $ forever $ reapWaiting watch chan `catch` \e -> 
        if isDoesNotExistError e
        then exitWith (ExitFailure (fromIntegral sig + 128))
        else throwIO e
