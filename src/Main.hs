{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Concurrent             (forkIO, newEmptyMVar, tryPutMVar, takeMVar, forkIO, threadDelay)
import           Control.Concurrent.STM         (TVar, atomically, newTVar, readTVar, modifyTVar')
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
import           System.Exit                    (ExitCode (ExitFailure), exitWith)
import           System.Posix.Process           (getAnyProcessStatus)
import           System.Posix.Signals           (Handler (Catch), installHandler, sigCHLD, sigINT, sigTERM, sigKILL,
                                                 signalProcess, setStoppedChildFlag, Signal)
import           System.Posix.Types             (ProcessID)
import           System.Process                 (callProcess)

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
    let delay = round . (* 1000000) <$> flip diffUTCTime now <$> next :: Maybe Integer
    let sleep = maybe maxSleep (max maxSleep) delay
    threadDelay (fromIntegral sleep)
    upd <- getCurrentTime
    if scheduleMatches sched upd
    then info ("Executing " <> unwords cmd) >> return (head cmd, tail cmd)
    else runConfiguration conf

tt :: IO ()
tt = pid1 $ getConfiguration (words "* * * * * /bin/date") >>= runConfiguration

main :: IO ()
main = do
    conf <- getArgs >>= getConfiguration
    when (runAtStart conf) $ callProcess (head $ command conf) (tail $ command conf)
    pid1 $ runConfiguration conf

{- pid1 package isn't general purpose enough for this, so below code is largely taken from there  -}

-- Given an action producing commands to be executed, pid1 will reap all children forever
pid1 :: IO (FilePath, [String]) -> IO ()
pid1 execf = do
    errno <- subreaper
    if errno /= 0
    then fail $ "error " <> show errno
    else pure ()

    exiting <- newEmptyMVar
    children <- atomically $ newTVar []

    -- Catch SIGTERM and SIGINT to exit all processes
    let startExiting = void . tryPutMVar exiting
    void $ installHandler sigTERM (Catch $ startExiting sigTERM) Nothing
    void $ installHandler sigINT  (Catch $ startExiting sigINT)  Nothing

    -- Catch SIGCHLD, ignoring stopped children
    void $ setStoppedChildFlag False
    void $ installHandler sigCHLD (Catch $ reapWaiting children `ifNotExistsThen` ()) Nothing

    -- Ask for the next process to be run
    void $ forkIO $ forever $ do
        (fp, args) <- execf
        callProcess fp args

    -- Then just wait forever for the `exiting` flag to be set.  This should happen on the main thread.
    sig <- takeMVar exiting
    void $ killAllChildren children sig

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
    print mstatus
    case mstatus of
        Nothing     -> pure ()
        Just status -> logErrorExit pids status

killAllChildren :: TVar [ProcessID] -> Signal -> IO ()
killAllChildren pids sig = do
    -- fork a process to send the TERM then KILL signals to all processes
    void $ forkIO $ flip ifNotExistsThen () $ do
        info "Sending all processes the TERM signal..."
        signalProcess sigTERM (-1)
        threadDelay $ 5 * 1000 * 1000
        info "Sending all processes the KILL signal..."
        signalProcess sigKILL (-1)

    -- reap all children until there are no more, then terminate
    void $ forever $ reapWaiting pids `catch` \e -> 
        if isDoesNotExistError e
        then exitWith (ExitFailure (fromIntegral sig + 128))
        else throwIO e
