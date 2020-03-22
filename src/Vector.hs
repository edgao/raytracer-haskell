-- I'm sure that the Linear module provides most/all of this functionality
-- but I don't want to figure out how to use it :P
module Vector where

import Linear.V3 (V3 (..))

dot :: V3 Double -> V3 Double -> Double
dot (V3 x1 y1 z1) (V3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2
(.*.) = dot

scale :: Double -> V3 Double -> V3 Double
scale scalar (V3 x y z) = V3 (scalar * x) (scalar * y) (scalar * z)
(***) = scale

add :: V3 Double -> V3 Double -> V3 Double
add (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1 + x2) (y1 + y2) (z1 + z2)
(+*+) = add

subtract :: V3 Double -> V3 Double -> V3 Double
subtract (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1 - x2) (y1 - y2) (z1 - z2)
(-*-) = Vector.subtract

norm :: V3 Double -> Double
norm (V3 x y z) = sqrt (x * x + y * y + z * z)

normalize :: V3 Double -> V3 Double
normalize v = scale (1 / norm v) v

reflect :: V3 Double -> V3 Double -> V3 Double
reflect incidentRay normalVector = incidentRay -*- ((2 * (incidentRay .*. normalVector)) *** normalVector)
