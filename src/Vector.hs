module Vector
    ( dot,
      scale,
      add
    ) where

import qualified Linear.V3                     as V

dot :: V.V3 Double -> V.V3 Double -> Double
dot (V.V3 x1 y1 z1) (V.V3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

scale :: Double -> V.V3 Double -> V.V3 Double
scale scalar (V.V3 x y z) = V.V3 (scalar * x) (scalar * y) (scalar * z)

add :: V.V3 Double -> V.V3 Double -> V.V3 Double
add (V.V3 x1 y1 z1) (V.V3 x2 y2 z2) = V.V3 (x1 + x2) (y1 + y2) (z1 + z2)
