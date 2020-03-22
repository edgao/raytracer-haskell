module Lib
    ( someFunc
    ) where

import Linear.V3
import qualified Vector as V

someFunc :: IO ()
someFunc = putStrLn "someFunc"

dot :: V3 Double -> V3 Double -> Double
dot (V3 x1 y1 z1) (V3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

data Shape = Triangle {a :: V3 Double, b :: V3 Double, c :: V3 Double}
           | Ellipsoid {center :: V3 Double, r1 :: V3 Double, r2 :: V3 Double, r3 :: V3 Double}

class Material a where
  hit :: a -> V3 Double -> V3 Double -> [(V3 Double, Double)]

newtype Mirror = Mirror {multiplier :: Double}
instance (Material Mirror) where
  hit (Mirror multiplier) incidentRay normalVector = [(
        V.add incidentRay (V.scale (-2 * V.dot incidentRay normalVector) normalVector),
        multiplier
     )]
