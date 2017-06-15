{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Reaper (subreaper) where

import Foreign.C

#include <sys/prctl.h>

foreign import ccall "sys/prctl.h prctl" prctl :: CInt -> CULong -> CULong -> CULong -> CULong -> IO CInt

subreaper :: IO Int
subreaper = do
    res <- prctl (#const PR_SET_CHILD_SUBREAPER) 1 0 0 0
    (Errno errno) <- getErrno
    return $ fromIntegral $ if res < 0 then errno else res
