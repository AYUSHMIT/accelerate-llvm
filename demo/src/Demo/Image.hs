{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Demo.Image
  ( blurImage
  , sobelEdges
  , loadImageAsArray
  , saveArrayAsImage
  ) where

import Data.Array.Accelerate                              as A
import qualified Codec.Picture                            as JP
import qualified Codec.Picture.Types                      as JP
import qualified Data.Vector.Storable                     as VS
import Data.Word                                          (Word8)

-- | Load a JuicyPixels image as an Accelerate array
loadImageAsArray :: JP.Image JP.PixelRGB8 -> Array DIM3 Word8
loadImageAsArray (JP.Image w h dat) =
  fromFunction (Z :. h :. w :. (3 :: Int)) $ \(Z :. y :. x :. c) ->
    dat VS.! (y * w * 3 + x * 3 + c)

-- | Save an Accelerate array as a JuicyPixels image
saveArrayAsImage :: Array DIM3 Word8 -> JP.Image JP.PixelRGB8
saveArrayAsImage arr =
  let Z :. h :. w :. _ = arrayShape arr
      pixels = VS.generate (w * h * 3) $ \i ->
        let y = (i `div` 3) `div` w
            x = (i `div` 3) `mod` w
            c = i `mod` 3
        in arr ! (Z :. y :. x :. c)
  in JP.Image w h pixels

-- | Simple box blur on a grayscale channel
blurChannel :: Acc (Array DIM2 Word8) -> Acc (Array DIM2 Word8)
blurChannel input =
  let
    Z :. h :. w = unlift (A.shape input) :: Z :. Exp Int :. Exp Int
    
    blurred = A.generate (A.shape input) $ \ix ->
      let Z :. y :. x = unlift ix :: Z :. Exp Int :. Exp Int
          
          -- 3x3 box blur kernel
          sum' = A.sum $ A.generate (constant (Z :. 3 :. 3)) $ \kix ->
            let Z :. ky :. kx = unlift kix :: Z :. Exp Int :. Exp Int
                ny = y + ky - 1
                nx = x + kx - 1
                
                -- Clamp to image boundaries
                ny' = A.max 0 (A.min (h - 1) ny)
                nx' = A.max 0 (A.min (w - 1) nx)
            in
              A.fromIntegral (input A.! A.index2 ny' nx')
          
          avg = A.fromIntegral sum' / (9.0 :: Exp Float)
      in
        A.round avg
  in
    blurred

-- | Apply blur to all channels of an RGB image
blurImage :: Acc (Array DIM3 Word8) -> Acc (Array DIM3 Word8)
blurImage input =
  let
    Z :. h :. w :. _ = unlift (A.shape input) :: Z :. Exp Int :. Exp Int :. Exp Int
    
    -- Extract and blur each channel
    blurredChannels = A.generate (constant (Z :. 3)) $ \cix ->
      let c = unindex1 cix
          channel = A.slice input (lift (Z :. All :. All :. c))
      in blurChannel channel
  in
    -- Reconstruct RGB image
    A.generate (A.shape input) $ \ix ->
      let Z :. y :. x :. c = unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
          channel = blurredChannels A.!! c
      in channel A.! A.index2 y x

-- | Sobel edge detection on grayscale channel
sobelChannel :: Acc (Array DIM2 Word8) -> Acc (Array DIM2 Word8)
sobelChannel input =
  let
    Z :. h :. w = unlift (A.shape input) :: Z :. Exp Int :. Exp Int
    
    edges = A.generate (A.shape input) $ \ix ->
      let Z :. y :. x = unlift ix :: Z :. Exp Int :. Exp Int
          
          -- Sobel kernels
          sobelX = [-1, 0, 1, -2, 0, 2, -1, 0, 1]
          sobelY = [-1, -2, -1, 0, 0, 0, 1, 2, 1]
          
          -- Compute gradients
          gx = A.sum $ A.generate (constant (Z :. 9)) $ \kix ->
            let ki = unindex1 kix
                ky = ki `div` 3 - 1
                kx = ki `mod` 3 - 1
                ny = A.max 0 (A.min (h - 1) (y + ky))
                nx = A.max 0 (A.min (w - 1) (x + kx))
                pix = A.fromIntegral (input A.! A.index2 ny nx) :: Exp Float
                kernel = sobelX !! ki
            in pix * A.fromIntegral (constant kernel :: Exp Int)
          
          gy = A.sum $ A.generate (constant (Z :. 9)) $ \kix ->
            let ki = unindex1 kix
                ky = ki `div` 3 - 1
                kx = ki `mod` 3 - 1
                ny = A.max 0 (A.min (h - 1) (y + ky))
                nx = A.max 0 (A.min (w - 1) (x + kx))
                pix = A.fromIntegral (input A.! A.index2 ny nx) :: Exp Float
                kernel = sobelY !! ki
            in pix * A.fromIntegral (constant kernel :: Exp Int)
          
          magnitude = sqrt (gx * gx + gy * gy)
          clamped = A.min 255 (A.max 0 (A.round magnitude))
      in
        clamped
  in
    edges

-- | Apply Sobel edge detection to an RGB image (convert to grayscale first)
sobelEdges :: Acc (Array DIM3 Word8) -> Acc (Array DIM3 Word8)
sobelEdges input =
  let
    Z :. h :. w :. _ = unlift (A.shape input) :: Z :. Exp Int :. Exp Int :. Exp Int
    
    -- Convert to grayscale
    grayscale = A.generate (A.index2 h w) $ \ix ->
      let Z :. y :. x = unlift ix :: Z :. Exp Int :. Exp Int
          r = A.fromIntegral (input A.! A.index3 y x 0) :: Exp Float
          g = A.fromIntegral (input A.! A.index3 y x 1) :: Exp Float
          b = A.fromIntegral (input A.! A.index3 y x 2) :: Exp Float
          gray = 0.299 * r + 0.587 * g + 0.114 * b
      in A.round gray
    
    -- Apply Sobel
    edges = sobelChannel grayscale
  in
    -- Convert back to RGB (all channels same)
    A.generate (A.shape input) $ \ix ->
      let Z :. y :. x :. _ = unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
      in edges A.! A.index2 y x
