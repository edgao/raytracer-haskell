module Material
    ( Material,
      hit,
      Mirror (..)
    ) where

import Linear.V3
import qualified Vector as V

class Material a where
  hit :: a -> V3 Double -> V3 Double -> [(V3 Double, Double)]

newtype Mirror = Mirror {multiplier :: Double}
instance (Material Mirror) where
  hit (Mirror multiplier) incidentRay normalVector = [(
        V.reflect incidentRay normalVector,
        multiplier
     )]
