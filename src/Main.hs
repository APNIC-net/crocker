{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Concurrent             (forkIO, newEmptyMVar, tryPutMVar, takeMVar, forkIO, threadDelay)
import           Control.Concurrent.STM         (TVar, atomically, newTVar, readTVar, modifyTVar')
import           Control.Exception              (catch, throwIO)
import           Control.Monad                  (void, forever)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.State            (StateT, evalStateT)
import           Control.Monad.Trans.Unlift     (MonadBaseUnlift, askUnliftBase, unliftBase)
import           Data.Attoparsec.Text           (parse, IResult (..))
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text)
import qualified Data.Text as T
import           Reaper
import           System.Cron
import           System.IO.Error                (isDoesNotExistError)
import           System.Environment             (getArgs)
import           System.Exit                    (ExitCode (ExitFailure), exitWith)
import           System.Posix.Process           (getAnyProcessStatus)
import           System.Posix.Signals           (Handler (Catch), installHandler, sigCHLD, sigINT, sigTERM, sigKILL,
                                                 signalProcess, setStoppedChildFlag, Signal)
import           System.Posix.Types             (ProcessID)

data Configuration = Configuration
    { runAtStart :: Bool
    , schedule   :: CronSchedule
    , command    :: [Text]
    } deriving Show

parseArgs :: [String] -> (Bool, [Text])
parseArgs ("--at-start":rest) = (True, T.pack <$> rest)
parseArgs ("-A":rest) = (True, T.pack <$> rest)
parseArgs rest = (False, T.pack <$> rest)

getConfiguration :: Monad m => [String] -> m Configuration
getConfiguration args = do
    let (ras, expr) = parseArgs args
    (cmd, sched) <- parseLoop expr (parse cronScheduleLoose)
    return $ Configuration ras sched cmd
  where
    parseLoop []     _ = fail "unexpected end of arguments while reading cron expression"
    parseLoop (t:ts) p = case p (t <> " ") of
        Fail x _ err -> fail $ err <> " while reading " <> T.unpack x
        Partial p'   -> parseLoop ts p'
        Done " " r   -> pure (ts, r)
        Done i _     -> fail $ "unrecognised input at end of cron expression: " <> T.unpack i

runConfiguration :: Configuration -> StateT Int IO (FilePath, [String])
runConfiguration (Configuration due sched cmd) = undefined

main :: IO ()
main = do
    conf <- getArgs >>= getConfiguration
    evalStateT (pid1 $ runConfiguration conf) 0

{- pid1 package isn't general purpose enough for this, so below code is largely taken from there  -}

voidIO :: (MonadBaseUnlift IO m, MonadIO m) => IO a -> m ()
voidIO = liftIO . void

-- Given an action producing commands to be executed, pid1 will reap all children forever
pid1 :: (MonadBaseUnlift IO m, MonadIO m) => m (FilePath, [String]) -> m ()
pid1 execf = do
    errno <- liftIO subreaper
    if errno /= 0
    then fail $ "error " <> show errno
    else pure ()

    exiting <- liftIO newEmptyMVar
    children <- liftIO . atomically $ newTVar []

    -- Catch SIGTERM and SIGINT to exit all processes
    let startExiting = void . tryPutMVar exiting
    voidIO $ installHandler sigTERM (Catch $ startExiting sigTERM) Nothing
    voidIO $ installHandler sigINT  (Catch $ startExiting sigINT)  Nothing

    -- Catch SIGCHLD, ignoring stopped children
    voidIO $ setStoppedChildFlag False
    voidIO $ installHandler sigCHLD (Catch $ reapWaiting children) Nothing

    -- Ask for the next process to be run
    u <- askUnliftBase
    voidIO $ forkIO $ forever $ do
        proc <- unliftBase u execf
        print proc

    -- Then just wait forever for the `exiting` flag to be set.  This should happen on the main thread.
    sig <- liftIO $ takeMVar exiting
    voidIO $ killAllChildren children sig

ifNotExistsThen :: IO a -> a -> IO a
ifNotExistsThen io a = io `catch` \e ->
    if isDoesNotExistError e
    then pure a
    else throwIO e

logErrorExit :: (Eq a, Show a, Show b) => TVar [a] -> (a, b) -> IO ()
logErrorExit pids (pid, status) = do
    p <- atomically $ do
        kids <- readTVar pids
        if pid `elem` kids
        then modifyTVar' pids (filter (/= pid)) >> pure (Just (pid, status))
        else pure Nothing
    mapM_ print p

-- waitpid() on all terminated child processes; log error exit code if it was a directly spawned child
reapWaiting :: TVar [ProcessID] -> IO ()
reapWaiting pids = do
    mstatus <- getAnyProcessStatus True False
    case mstatus of
        Nothing     -> pure ()
        Just status -> logErrorExit pids status

killAllChildren :: TVar [ProcessID] -> Signal -> IO ()
killAllChildren pids sig = flip ifNotExistsThen () $ do
    -- fork a process to send the TERM then KILL signals to all processes
    void $ forkIO $ do
        signalProcess sigTERM (-1)
        threadDelay $ 5 * 1000 * 1000
        signalProcess sigKILL (-1)

    -- reap all children until there are no more, then terminate
    void $ forever reapWaiting pids `ifNotExistsThen` exitWith (ExitFailure (fromIntegral sig + 128))
