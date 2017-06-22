module Main (main) where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Crocker

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
    ]

main :: IO ()
main = defaultMain tests
