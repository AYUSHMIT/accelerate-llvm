{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Demo.NBody
  ( nBodyStep
  , initRandomBodies
  , renderBodiesFrame
  ) where

import Data.Array.Accelerate                              as A
import qualified Codec.Picture                            as JP
import qualified Codec.Picture.Types                      as JP
import qualified Data.Vector.Storable                     as VS
import Data.Word                                          (Word8)

-- | Body representation: (x, y, vx, vy, mass)
type Body = (Float, Float, Float, Float, Float)

-- | Gravitational constant (scaled for simulation)
gConst :: Exp Float
gConst = 0.001

-- | Time step for simulation
dt :: Exp Float
dt = 0.016  -- ~60 FPS

-- | Softening parameter to prevent singularities
softening :: Exp Float
softening = 0.1

-- | Compute acceleration on body i due to body j
accel :: Exp Body -> Exp Body -> Exp (Float, Float)
accel bi bj =
  let (xi, yi, _, _, _) = unlift bi
      (xj, yj, _, _, mj) = unlift bj
      
      dx = xj - xi
      dy = yj - yi
      distSq = dx * dx + dy * dy + softening * softening
      dist = sqrt distSq
      
      force = gConst * mj / (distSq * dist)
      
      ax = force * dx
      ay = force * dy
  in
    lift (ax, ay)

-- | Single step of N-body simulation
nBodyStep :: Acc (Vector Body) -> Acc (Vector Body)
nBodyStep bodies =
  let
    n = A.length bodies
    
    -- Compute accelerations for each body
    accs = A.generate (A.shape bodies) $ \ix ->
      let i = unindex1 ix
          bi = bodies A.!! i
          
          -- Sum accelerations from all other bodies
          totalAcc = A.fold
            (\acc j ->
              let aj = accel bi (bodies A.!! j)
                  (ax1, ay1) = unlift acc
                  (ax2, ay2) = unlift aj
              in lift (ax1 + ax2, ay1 + ay2))
            (lift (0.0, 0.0))
            (A.enumFromN (A.index1 n) n)
      in totalAcc
    
    -- Update velocities and positions
    updated = A.zipWith updateBody bodies accs
  in
    updated
  where
    updateBody :: Exp Body -> Exp (Float, Float) -> Exp Body
    updateBody body acc =
      let (x, y, vx, vy, m) = unlift body
          (ax, ay) = unlift acc
          
          vx' = vx + ax * dt
          vy' = vy + ay * dt
          
          x' = x + vx' * dt
          y' = y + vy' * dt
      in
        lift (x', y', vx', vy', m)

-- | Initialize random bodies (placeholder - in reality would use random data)
initRandomBodies :: Int -> Array DIM1 Body
initRandomBodies n =
  fromList (Z :. n) 
    [ ( cos (fromIntegral i * 2.0 * pi / fromIntegral n) * 0.3
      , sin (fromIntegral i * 2.0 * pi / fromIntegral n) * 0.3
      , -sin (fromIntegral i * 2.0 * pi / fromIntegral n) * 0.1
      , cos (fromIntegral i * 2.0 * pi / fromIntegral n) * 0.1
      , 1.0
      )
    | i <- [0..n-1]
    ]

-- | Render bodies to an image frame
renderBodiesFrame :: Int -> Int -> Array DIM1 Body -> JP.Image JP.PixelRGB8
renderBodiesFrame width height bodies =
  let
    Z :. n = arrayShape bodies
    
    -- Create blank canvas
    pixels = VS.generate (width * height * 3) $ \i ->
      let pixelIdx = i `div` 3
          component = i `mod` 3
          y = pixelIdx `div` width
          x = pixelIdx `mod` width
          
          -- Check if any body is near this pixel
          hasBody = any (isNearPixel x y) [0..n-1]
      in
        if hasBody
        then case component of
               0 -> 255  -- R
               1 -> 255  -- G
               2 -> 100  -- B (yellow-ish)
        else 0  -- Black background
  in
    JP.Image width height pixels
  where
    isNearPixel :: Int -> Int -> Int -> Bool
    isNearPixel px py idx =
      let (x, y, _, _, _) = bodies ! (Z :. idx)
          -- Convert from simulation space [-1, 1] to pixel space
          screenX = floor $ (x + 1.0) * fromIntegral width / 2.0
          screenY = floor $ (y + 1.0) * fromIntegral height / 2.0
          radius = 3  -- Body rendering radius in pixels
          dx = px - screenX
          dy = py - screenY
      in dx * dx + dy * dy <= radius * radius
