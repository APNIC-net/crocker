{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Reaper (subreaper) where

import qualified Language.C.Inline as C

C.include "<sys/prctl.h>"
C.include "<errno.h>"

subreaper :: IO Int
subreaper = fromIntegral <$> [C.block| int
    { 
        return prctl(PR_SET_CHILD_SUBREAPER, 1, 0, 0, 0) < 0 ? errno : 0;
    } |]
