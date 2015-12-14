{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Compile
-- Copyright   : [2014..2015] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Compile (

  module Data.Array.Accelerate.LLVM.Compile,
  ExecutableR(..), Kernel(..),

) where

-- llvm-general
import LLVM.General.Context                                     ( Context )
import LLVM.General.AST                                         hiding ( Module )
import LLVM.General.AST.Global
import qualified LLVM.General.AST                               as AST
import qualified LLVM.General.Analysis                          as LLVM
import qualified LLVM.General.Module                            as LLVM
import qualified LLVM.General.PassManager                       as LLVM

-- accelerate
import Data.Array.Accelerate.Error                              ( internalError )
import Data.Array.Accelerate.Trafo                              ( DelayedOpenAcc )

import Data.Array.Accelerate.LLVM.CodeGen
import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma )
import Data.Array.Accelerate.LLVM.CodeGen.Module                ( Module(..) )
import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.PTX.Compile.Link
import Data.Array.Accelerate.LLVM.PTX.CodeGen                  ( )
import Data.Array.Accelerate.LLVM.PTX.Analysis.Launch

import qualified  Data.Array.Accelerate.LLVM.PTX.Debug          as Debug

-- cuda
import qualified Foreign.CUDA.Analysis                          as CUDA
import qualified Foreign.CUDA.Driver                            as CUDA
#ifdef ACCELERATE_USE_LIBNVVM
import qualified Foreign.LibNVVM                                as NVVM
#endif

-- standard library
import Data.ByteString                                          ( ByteString )
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Text.Printf
import qualified Data.ByteString.Char8                          as B


instance Compile PTX where
  data ExecutableR PTX = PTXR { ptxKernel :: [Kernel]
                              , ptxModule :: {-# UNPACK #-} !CUDA.Module
                              }
  compileForTarget     = compileForPTX


data Kernel = Kernel {
    kernelFun                   :: {-# UNPACK #-} !CUDA.Fun
  , kernelOccupancy             :: {-# UNPACK #-} !CUDA.Occupancy
  , kernelSharedMemBytes        :: {-# UNPACK #-} !Int
  , kernelThreadBlockSize       :: {-# UNPACK #-} !Int
  , kernelThreadBlocks          :: (Int -> Int)
  , kernelName                  :: String
  }

-- | Compile a given module for the NVPTX backend. This produces a CUDA module
-- as well as a list of the kernel functions in the module, together with some
-- occupancy information.
--
compileForPTX
    :: DelayedOpenAcc aenv a
    -> Gamma aenv
    -> LLVM PTX (ExecutableR PTX)
compileForPTX acc aenv = do
  target <- gets llvmTarget
  ctx    <- asks llvmContext
  let Module ast = llvmOfOpenAcc target acc aenv
      dev        = ptxDeviceProperties target
      globals    = globalFunctions (moduleDefinitions ast)

  liftIO $ do
    ptx  <- compileModule dev ctx ast
    funs <- sequence [ linkFunction acc dev ptx f | f <- globals ]
    return $! PTXR funs ptx


-- | Compile the LLVM module to produce a CUDA module.
--
--    * If we are using libNVVM, this includes all LLVM optimisations plus some
--    sekrit optimisations.
--
--    * If we are just using the llvm ptx backend, we still need to run the
--    standard optimisations.
--
compileModule :: CUDA.DeviceProperties -> Context -> AST.Module -> IO CUDA.Module
compileModule dev ctx ast =
  let name      = moduleName ast in
#ifdef ACCELERATE_USE_LIBNVVM
  withLibdeviceNVVM  dev ctx ast (compileModuleNVVM  dev name)
#else
  withLibdeviceNVPTX dev ctx ast (compileModuleNVPTX dev name)
#endif


#ifdef ACCELERATE_USE_LIBNVVM
-- Compiling the module with libNVVM implicitly uses LLVM-3.2.
--
compileModuleNVVM :: CUDA.DeviceProperties -> String -> [(String, ByteString)] -> LLVM.Module -> IO CUDA.Module
compileModuleNVVM dev name libdevice mdl = do
  -- Lower the module to bitcode and have libNVVM compile to PTX
  let arch      = CUDA.computeCapability dev
      verbose   = if Debug.mode Debug.debug then [ NVVM.GenerateDebugInfo ] else []
      flags     = NVVM.Target arch : verbose

#ifdef ACCELERATE_INTERNAL_CHECKS
      verify  = True
#else
      verify  = False
#endif

  ll    <- LLVM.moduleString mdl        -- no LLVM.moduleBitcode in llvm-general-3.2.*
  ptx   <- NVVM.compileModules ((name,B.pack ll):libdevice) flags verify

  -- Debugging
  Debug.when Debug.dump_cc $ do
    Debug.traceIO Debug.verbose ll
    Debug.traceIO Debug.dump_cc $ "llvm: " ++ B.unpack (NVVM.nvvmLog ptx)

  -- Link into a new CUDA module in the current context
  linkPTX name (NVVM.nvvmResult ptx)

#else
-- Compiling with the NVPTX backend uses LLVM-3.3 and above
--
compileModuleNVPTX :: CUDA.DeviceProperties -> String -> LLVM.Module -> IO CUDA.Module
compileModuleNVPTX dev name mdl =
  withPTXTargetMachine dev $ \nvptx -> do

    -- Run the standard optimisation pass
    --
    -- NOTE: Currently we require keeping this at level 2, otherwise incorrect
    --       code is generated for multidimensional folds.
    --
    let pss        = LLVM.defaultCuratedPassSetSpec { LLVM.optLevel = Just 2 }
        runError e = either ($internalError "compileModuleNVPTX") id `fmap` runExceptT e

    LLVM.withPassManager pss $ \pm -> do
#ifdef ACCELERATE_INTERNAL_CHECKS
      runError $ LLVM.verify mdl
#endif
      b1      <- LLVM.runPassManager pm mdl

      -- debug printout
      Debug.when Debug.dump_cc $ do
        Debug.traceIO Debug.dump_cc $ printf "llvm: optimisation did work? %s" (show b1)
        Debug.traceIO Debug.verbose =<< LLVM.moduleLLVMAssembly mdl

      -- Lower the LLVM module into target assembly (PTX)
      ptx <- runError (LLVM.moduleTargetAssembly nvptx mdl)

      -- Link into a new CUDA module in the current context
      linkPTX name (B.pack ptx)
#endif

-- | Load the given CUDA PTX into a new module that is linked into the current
-- context.
--
linkPTX :: String -> ByteString -> IO CUDA.Module
linkPTX name ptx = do
  _verbose      <- Debug.queryFlag Debug.verbose
  _debug        <- Debug.queryFlag Debug.debug_cc
  --
  let v         = if _verbose then [ CUDA.Verbose ]                                  else []
      d         = if _debug   then [ CUDA.GenerateDebugInfo, CUDA.GenerateLineInfo ] else []
      flags     = concat [v,d]
  --
  jit   <- CUDA.loadDataEx ptx flags

  Debug.when Debug.dump_asm $ do
    Debug.traceIO Debug.verbose (B.unpack ptx)
    Debug.traceIO Debug.dump_asm $
      printf "ptx: compiled entry function \"%s\" in %s\n%s"
             name
             (Debug.showFFloatSIBase (Just 2) 1000 (CUDA.jitTime jit / 1000) "s")
             (B.unpack (CUDA.jitInfoLog jit))

  return $! CUDA.jitModule jit


-- | Extract the named function from the module and package into a Kernel
-- object, which includes meta-information on resource usage.
--
-- If we are in debug mode, print statistics on kernel resource usage, etc.
--
linkFunction
    :: DelayedOpenAcc aenv a
    -> CUDA.DeviceProperties            -- device properties
    -> CUDA.Module                      -- the compiled module
    -> String                           -- __global__ entry function name
    -> IO Kernel
linkFunction acc dev mdl name = do
  f     <- CUDA.getFun mdl name
  regs  <- CUDA.requires f CUDA.NumRegs
  ssmem <- CUDA.requires f CUDA.SharedSizeBytes
  cmem  <- CUDA.requires f CUDA.ConstSizeBytes
  lmem  <- CUDA.requires f CUDA.LocalSizeBytes
  maxt  <- CUDA.requires f CUDA.MaxKernelThreadsPerBlock

  let occ               = determineOccupancy acc dev maxt regs ssmem
      (cta,blocks,smem) = launchConfig acc dev occ

      msg1, msg2 :: String
      msg1 = printf "entry function '%s' used %d registers, %d bytes smem, %d bytes lmem, %d bytes cmem"
                      name regs smem lmem cmem

      msg2 = printf "multiprocessor occupancy %.1f %% : %d threads over %d warps in %d blocks"
                      (CUDA.occupancy100 occ)
                      (CUDA.activeThreads occ)
                      (CUDA.activeWarps occ)
                      (CUDA.activeThreadBlocks occ)

  Debug.traceIO Debug.dump_cc (printf "cc: %s\n  ... %s" msg1 msg2)
  return $ Kernel f occ smem cta blocks name


-- | Extract the names of the function definitions from the module.
--
-- Note: [Extracting global function names]
--
-- It is important to run this on the module given to us by code generation.
-- After combining modules with 'libdevice', extra function definitions,
-- corresponding to basic maths operations, will be added to the module. These
-- functions will not be callable as __global__ functions.
--
-- The list of names will be exported in the order that they appear in the
-- module.
--
globalFunctions :: [Definition] -> [String]
globalFunctions defs =
  [ n | GlobalDefinition Function{..} <- defs
      , not (null basicBlocks)
      , let Name n = name
      ]

