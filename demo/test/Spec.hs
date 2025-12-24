module Main where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native as CPU

import qualified Demo.Mandelbrot as Mandelbrot
import qualified Demo.NBody as NBody
import Demo.Backend

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Demo Tests"
  [ testGroup "Backend"
    [ testCase "Select CPU backend" $ do
        backend <- selectBackend CPU
        case backend of
          NativeBackend -> return ()
    , testCase "Select Auto backend" $ do
        backend <- selectBackend Auto
        case backend of
          NativeBackend -> return ()
    ]
  
  , testGroup "Mandelbrot"
    [ testCase "Generate small Mandelbrot" $ do
        let result = CPU.run $ Mandelbrot.mandelbrotImage 16 16 10 (-2.5) 1.0 (-1.0) 1.0
            A.Z A.:. h A.:. w = A.arrayShape result
        assertEqual "Width should be 16" 16 w
        assertEqual "Height should be 16" 16 h
    ]
  
  , testGroup "N-body"
    [ testCase "Initialize bodies" $ do
        let bodies = NBody.initRandomBodies 10
            A.Z A.:. n = A.arrayShape bodies
        assertEqual "Should have 10 bodies" 10 n
    
    , testCase "Single N-body step" $ do
        let bodies = NBody.initRandomBodies 10
            result = CPU.run $ NBody.nBodyStep (A.use bodies)
            A.Z A.:. n = A.arrayShape result
        assertEqual "Should still have 10 bodies" 10 n
    ]
  ]
