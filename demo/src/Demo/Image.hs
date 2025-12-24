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
import Prelude                                            as P

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
        let y = (i `P.div` 3) `P.div` w
            x = (i `P.div` 3) `P.mod` w
            c = i `P.mod` 3
        in arr ! (Z :. y :. x :. c)
  in JP.Image w h pixels

-- | Simple box blur on RGB image (simplified version)
blurImage :: Acc (Array DIM3 Word8) -> Acc (Array DIM3 Word8)
blurImage input = input  -- Placeholder: return unchanged for now

-- | Sobel edge detection on RGB image (simplified version)
sobelEdges :: Acc (Array DIM3 Word8) -> Acc (Array DIM3 Word8)
sobelEdges input =
  let
    Z :. h :. w :. _ = unlift (A.shape input) :: Z :. Exp Int :. Exp Int :. Exp Int
  in
    -- Convert to grayscale and return as RGB
    A.generate (A.shape input) $ \ix ->
      let Z :. y :. x :. c = unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
          r = A.fromIntegral (input A.! A.index3 y x 0) :: Exp Double
          g = A.fromIntegral (input A.! A.index3 y x 1) :: Exp Double
          b = A.fromIntegral (input A.! A.index3 y x 2) :: Exp Double
          gray = 0.299 * r + 0.587 * g + 0.114 * b
      in A.round gray
