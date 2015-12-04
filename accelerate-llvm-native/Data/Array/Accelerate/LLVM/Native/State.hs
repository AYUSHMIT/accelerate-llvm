{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.State
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.State (

  evalNative,
  createTarget, defaultTarget,

) where

-- accelerate
import Control.Parallel.Meta
import Control.Parallel.Meta.Worker
import qualified Control.Parallel.Meta.Trans.LBS                as LBS
import qualified Control.Parallel.Meta.Resource.SMP             as SMP
import qualified Control.Parallel.Meta.Resource.Single          as Single

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Native.Target

import qualified Data.Array.Accelerate.LLVM.Native.Debug        as Debug

-- library
import System.IO.Unsafe
import Text.Printf

import GHC.Conc


-- | Execute a computation in the Native backend
--
evalNative :: Native -> LLVM Native a -> IO a
evalNative = evalLLVM


-- | Create a Native execution target for the given capabilities.
--
-- This spawns a worker thread on the specified CPUs. A lazy binary splitting
-- work-stealing scheduler is used to balance the load amongst the available
-- processors. A suitable PPT should be chosen when invoking the continuation in
-- order to balance scheduler overhead with fine-grained function calls.
--
createTarget
    :: [Int]
    -> IO Native
createTarget caps = do
  gang   <- forkGangOn caps
  return $! Native gang (balancedParIO gang)


-- | Execute a computation without load balancing. Each thread evaluates its
-- local work queue only and does not create or search for additional work. This
-- relies on 'runParIO' initialising each thread with an equally sized chunk of
-- the input.
--
_unbalancedParIO :: Gang -> Executable
_unbalancedParIO gang =
  Executable $ \_ range after fill ->
    timed $ runParIO Single.mkResource gang range fill (runFinalise after)


-- | Execute a computation using lazy splitting work stealing queues.
--
balancedParIO :: Gang -> Executable
balancedParIO gang =
  Executable $ \ppt range after fill ->
    let retries  = gangSize gang
        resource = LBS.mkResource ppt $ SMP.mkResource retries gang
    in
    timed $ runParIO resource gang range fill (runFinalise after)


-- Top-level mutable state
-- -----------------------
--
-- It is important to keep some information alive for the entire run of the
-- program, not just a single execution. These tokens use 'unsafePerformIO' to
-- ensure they are executed only once, and reused for subsequent invocations.
--

-- | Initialise the gang of threads that will be used to execute computations.
-- This spawns one worker on each capability, which can be set via +RTS -Nn.
--
-- This globally shared thread gang is auto-initialised on startup and shared by
-- all computations (unless the user chooses to 'run' with a different gang).
--
-- In a data parallel setting, it does not help to have multiple gangs running
-- at the same time. This is because a single data parallel computation should
-- already be able to keep all threads busy. If we had multiple gangs running at
-- the same time, then the system as a whole would run slower as the gangs
-- contend for cache and thrash the scheduler.
--
{-# NOINLINE defaultTarget #-}
defaultTarget :: Native
defaultTarget = unsafePerformIO $ do
  Debug.traceIO Debug.dump_gc (printf "gc: initialise native target with %d CPUs" numCapabilities)
  createTarget [0 .. numCapabilities - 1]


-- Debugging
-- ---------

{-# INLINE timed #-}
timed :: IO a -> IO a
timed f = Debug.timed Debug.dump_exec elapsed f

{-# INLINE elapsed #-}
elapsed :: Double -> Double -> String
elapsed x y = "exec: " ++ Debug.elapsed x y

