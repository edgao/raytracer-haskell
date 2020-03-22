-- I'm sure that the Linear module provides most/all of this functionality
-- but I don't want to figure out how to use it :P
module Vector where

import qualified Linear.V3                     as V

dot :: V.V3 Double -> V.V3 Double -> Double
dot (V.V3 x1 y1 z1) (V.V3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

scale :: Double -> V.V3 Double -> V.V3 Double
scale scalar (V.V3 x y z) = V.V3 (scalar * x) (scalar * y) (scalar * z)

add :: V.V3 Double -> V.V3 Double -> V.V3 Double
add (V.V3 x1 y1 z1) (V.V3 x2 y2 z2) = V.V3 (x1 + x2) (y1 + y2) (z1 + z2)

norm :: V.V3 Double -> Double
norm (V.V3 x y z) = sqrt (x * x + y * y + z * z)

normalize :: V.V3 Double -> V.V3 Double
normalize v = scale (1 / norm v) v

reflect :: V.V3 Double -> V.V3 Double -> V.V3 Double
reflect incidentRay normalVector = add incidentRay (scale (-2 * dot incidentRay normalVector) normalVector)
