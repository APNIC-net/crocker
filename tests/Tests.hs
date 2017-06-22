module Main (main) where

import Control.Concurrent.STM
import Control.Monad
import Control.Exception (bracket)
import System.Posix.Signals
import System.Timeout
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Crocker

reapChild :: Assertion
reapChild = do
    pids <- atomically $ newTChan
    send <- atomically $ newTVar True
    void $ setStoppedChildFlag False
    result <- timeout (1 * 1000 * 1000) $ bracket
        (installHandler sigCHLD (Catch $ reapWaiting send pids) Nothing)
        (\h -> installHandler sigCHLD h Nothing)
        $ \_ -> do
            pid <- execute "/bin/false" []
            waitPid pid pids
    case result of
        Nothing -> assertFailure "Failed to receive notification of child process termination"
        Just _  -> pure ()

dontReapChild :: Assertion
dontReapChild = do
    pids <- atomically $ newTChan
    send <- atomically $ newTVar False
    void $ setStoppedChildFlag False
    result <- timeout (1 * 1000 * 1000) $ bracket
        (installHandler sigCHLD (Catch $ reapWaiting send pids) Nothing)
        (\h -> installHandler sigCHLD h Nothing)
        $ \_ -> do
            pid <- execute "/bin/false" []
            waitPid pid pids
    case result of
        Nothing -> pure ()
        Just _  -> assertFailure "Received unexpected notification of child termination"
    isEmpty <- atomically $ isEmptyTChan pids
    assertBool "Child termination should not be recorded" isEmpty

tests :: [Test.Framework.Test]
tests = 
    [ testGroup "configuration parsing"
        [ testCase "daily 1" (command <$> getConfiguration ["@daily", "/bin/false"] @?= Just ["/bin/false"])
        , testCase "daily 2" (command <$> getConfiguration ["-A", "@daily", "/bin/false"] @?= Just ["/bin/false"])
        , testCase "daily 3" ((show . schedule) <$> getConfiguration ["@daily"] @?= Just "CronSchedule 0 0 * * *")
        , testCase "at start 1" (runAtStart <$> getConfiguration ["-A", "* * * * *"] @?= Just True)
        , testCase "at start 2" (runAtStart <$> getConfiguration ["--at-start", "* * * * *"] @?= Just True)
        , testCase "not at start" (runAtStart <$> getConfiguration ["* * * * *"] @?= Just False)
        ]
    , testGroup "subprocess reaper"
        [ testCase "reap child" reapChild
        , testCase "don't reap child" dontReapChild
        ]
    ]

main :: IO ()
main = defaultMain tests
