{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Demo.Mandelbrot
  ( mandelbrot
  , renderMandelbrot
  , mandelbrotImage
  ) where

import Data.Array.Accelerate                              as A
import Data.Array.Accelerate.Data.Complex                 as A
import qualified Codec.Picture                            as JP
import qualified Codec.Picture.Types                      as JP
import qualified Data.Vector.Storable                     as VS
import Data.Word                                          (Word8)

-- | Mandelbrot set computation parameters
type R = Float

-- | Compute the Mandelbrot set
mandelbrot
    :: Exp Int                  -- ^ maximum iteration depth
    -> Exp R                    -- ^ view x-min
    -> Exp R                    -- ^ view x-max
    -> Exp R                    -- ^ view y-min
    -> Exp R                    -- ^ view y-max
    -> Exp (Complex R)          -- ^ current point
    -> Exp Int
mandelbrot depth xmin xmax ymin ymax c = 
  let
    -- Scale point to view coordinates
    x = xmin + A.real c * (xmax - xmin)
    y = ymin + A.imag c * (ymax - ymin)
    z0 = lift (x :+ y)
  in
    A.snd $ A.while
      (\zi -> let (_, i) = unlift zi
              in  i < depth A.&& magnitude2 (A.fst zi) < 4.0)
      (\zi -> let (z, i) = unlift zi
              in  lift (z * z + z0, i + 1))
      (lift (z0, constant 0 :: Exp Int))

-- | Magnitude squared of a complex number
magnitude2 :: Exp (Complex R) -> Exp R
magnitude2 z = 
  let r = A.real z
      i = A.imag z
  in r * r + i * i

-- | Generate the Mandelbrot set for a given view window
mandelbrotImage
    :: Int                      -- ^ Image width
    -> Int                      -- ^ Image height
    -> Int                      -- ^ Maximum iterations
    -> R                        -- ^ View x-min
    -> R                        -- ^ View x-max
    -> R                        -- ^ View y-min
    -> R                        -- ^ View y-max
    -> Acc (Array DIM2 Int)
mandelbrotImage width height depth xmin xmax ymin ymax =
  let
    w = constant width
    h = constant height
    d = constant depth
    
    -- Generate grid of complex numbers
    points :: Acc (Array DIM2 (Complex R))
    points = A.generate (constant (Z :. height :. width)) $ \ix ->
      let Z :. y :. x = unlift ix :: Z :. Exp Int :. Exp Int
          fx = A.fromIntegral x / A.fromIntegral w
          fy = A.fromIntegral y / A.fromIntegral h
      in lift (fx :+ fy)
  in
    A.map (mandelbrot d (constant xmin) (constant xmax) (constant ymin) (constant ymax)) points

-- | Map iteration count to RGB color using a simple palette
iterToRGB :: Int -> Int -> (Word8, Word8, Word8)
iterToRGB maxIter iter
  | iter >= maxIter = (0, 0, 0)
  | otherwise =
      let t = fromIntegral iter / fromIntegral maxIter
          r = floor $ 255 * (0.5 + 0.5 * cos (3.0 * pi * t))
          g = floor $ 255 * (0.5 + 0.5 * cos (3.0 * pi * t + 2.0 * pi / 3.0))
          b = floor $ 255 * (0.5 + 0.5 * cos (3.0 * pi * t + 4.0 * pi / 3.0))
      in (r, g, b)

-- | Render Mandelbrot array to a JuicyPixels image
renderMandelbrot :: Int -> Array DIM2 Int -> JP.Image JP.PixelRGB8
renderMandelbrot maxIter arr =
  let Z :. h :. w = arrayShape arr
      pixels = VS.generate (w * h * 3) $ \i ->
        let pixelIdx = i `div` 3
            component = i `mod` 3
            y = pixelIdx `div` w
            x = pixelIdx `mod` w
            iter = arr ! (Z :. y :. x)
            (r, g, b) = iterToRGB maxIter iter
        in case component of
             0 -> r
             1 -> g
             _ -> b
  in JP.Image w h pixels
