{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Prelude                                            as P

-- | Body representation: (x, y, vx, vy, mass)
type Body = (Float, Float, Float, Float, Float)

-- | Gravitational constant (scaled for simulation)
gConst :: Exp Float
gConst = 0.001

-- | Time step for simulation
dt :: Exp Float
dt = 0.016  -- ~60 FPS

-- | Single step of N-body simulation (simplified version)
nBodyStep :: Acc (Vector Body) -> Acc (Vector Body)
nBodyStep bodies =
  A.map updateBody bodies
  where
    -- Update each body independently (simplified - no inter-body forces for now)
    updateBody :: Exp Body -> Exp Body
    updateBody body =
      let (x :: Exp Float, y :: Exp Float, vx :: Exp Float, vy :: Exp Float, m :: Exp Float) = unlift body
          
          -- Simple damping to keep bodies from flying away
          vx' = vx * 0.99 :: Exp Float
          vy' = vy * 0.99 :: Exp Float
          
          x' = x + vx' * dt :: Exp Float
          y' = y + vy' * dt :: Exp Float
          
          -- Wrap around walls instead of bouncing
          x'' = A.cond (x' A.> 1.0) (x' - 2.0) (A.cond (x' A.< -1.0) (x' + 2.0) x')
          y'' = A.cond (y' A.> 1.0) (y' - 2.0) (A.cond (y' A.< -1.0) (y' + 2.0) y')
      in
        lift (x'' :: Exp Float, y'' :: Exp Float, vx' :: Exp Float, vy' :: Exp Float, m :: Exp Float)

-- | Initialize random bodies (placeholder - in reality would use random data)
initRandomBodies :: Int -> Array DIM1 Body
initRandomBodies n =
  fromList (Z :. n) 
    [ ( P.cos (P.fromIntegral i * 2.0 * P.pi / P.fromIntegral n) * 0.3
      , P.sin (P.fromIntegral i * 2.0 * P.pi / P.fromIntegral n) * 0.3
      , -P.sin (P.fromIntegral i * 2.0 * P.pi / P.fromIntegral n) * 0.1
      , P.cos (P.fromIntegral i * 2.0 * P.pi / P.fromIntegral n) * 0.1
      , 1.0
      )
    | i <- [0..n-1]
    ]

-- | Render bodies to an image frame
renderBodiesFrame :: Int -> Int -> Array DIM1 Body -> JP.Image JP.PixelRGB8
renderBodiesFrame width height bodies =
  let
    Z :. n = arrayShape bodies
    
    -- Extract body positions into a list for rendering
    bodyList = [(x, y) | (x, y, _, _, _) <- toList bodies]
    
    -- Create blank canvas
    pixels = VS.generate (width * height * 3) $ \i ->
      let pixelIdx = i `P.div` 3
          component = i `P.mod` 3
          y = pixelIdx `P.div` width
          x = pixelIdx `P.mod` width
          
          -- Check if any body is near this pixel
          hasBody = P.any (isNearPixel x y) bodyList
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
    isNearPixel :: Int -> Int -> (Float, Float) -> Bool
    isNearPixel px py (bx, by) =
      let -- Convert from simulation space [-1, 1] to pixel space
          screenX = P.floor $ (bx + 1.0) * P.fromIntegral width / 2.0
          screenY = P.floor $ (by + 1.0) * P.fromIntegral height / 2.0
          radius = 3  -- Body rendering radius in pixels
          dx = px - screenX
          dy = py - screenY
      in dx * dx + dy * dy P.<= radius * radius
