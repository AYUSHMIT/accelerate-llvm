{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Control.Parallel.Meta.Resource.Single
-- Copyright   : [2014..2015] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Control.Parallel.Meta.Resource.Single
  where

-- accelerate
import Control.Parallel.Meta
import Control.Parallel.Meta.Worker

-- library
import Data.Monoid
import Data.Concurrent.Deque.Class
import Prelude


-- | Create a resource where each thread works in isolation. The resource is not
-- aware of any other sources of work (at this level) and only ever tries to pop
-- from its own local queue.
--
mkResource :: Resource
mkResource = Resource mempty (WorkSearch (tryPopL . workpool))

