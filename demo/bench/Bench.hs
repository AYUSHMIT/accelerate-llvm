module Main where

import Criterion.Main
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native as CPU

import qualified Demo.Mandelbrot as Mandelbrot
import qualified Demo.NBody as NBody

main :: IO ()
main = defaultMain
  [ bgroup "Mandelbrot"
    [ bench "512x384/CPU"  $ nf (CPU.run . mandelbrot512) ()
    , bench "1024x768/CPU" $ nf (CPU.run . mandelbrot1024) ()
    ]
  , bgroup "N-body"
    [ bench "100-particles/CPU"  $ nf (CPU.run . nbody100) ()
    , bench "500-particles/CPU"  $ nf (CPU.run . nbody500) ()
    ]
  ]
  where
    mandelbrot512 _ = Mandelbrot.mandelbrotImage 512 384 100 (-2.5) 1.0 (-1.0) 1.0
    mandelbrot1024 _ = Mandelbrot.mandelbrotImage 1024 768 100 (-2.5) 1.0 (-1.0) 1.0
    
    nbody100 _ = NBody.nBodyStep (A.use $ NBody.initRandomBodies 100)
    nbody500 _ = NBody.nBodyStep (A.use $ NBody.initRandomBodies 500)
