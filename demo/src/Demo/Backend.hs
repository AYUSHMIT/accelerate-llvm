{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Demo.Backend
  ( Backend(..)
  , BackendChoice(..)
  , selectBackend
  , runWithBackend
  ) where

import Data.Array.Accelerate                              ( Arrays )
import Data.Array.Accelerate.LLVM.Native                  as CPU
import qualified Data.Array.Accelerate                    as A

-- | Available backend choices
data BackendChoice
  = Auto    -- ^ Automatically select best available backend
  | CPU     -- ^ Force CPU backend (llvm-native)
  deriving (Eq, Show)

-- | Selected backend with execution capability
data Backend where
  NativeBackend :: Backend

-- | Select backend based on user preference
selectBackend :: BackendChoice -> IO Backend
selectBackend CPU = do
  putStrLn "Using CPU backend (accelerate-llvm-native)"
  return NativeBackend
selectBackend Auto = do
  -- For now, we only support CPU backend in the basic demo
  -- GPU support (accelerate-llvm-ptx) can be added later
  putStrLn "Auto-detecting backend: CPU (accelerate-llvm-native)"
  return NativeBackend

-- | Run an Accelerate computation with the selected backend
runWithBackend :: Arrays a => Backend -> A.Acc a -> IO a
runWithBackend NativeBackend = return . CPU.run
