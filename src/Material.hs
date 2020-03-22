module Material
    ( Color (..),
      ReflectionStrategy (..),
      hit
    ) where

import Linear.V3 (V3 (..))
import qualified Vector as V

-- All values are between 0 and 1
data Color = Color {
  r :: Double,
  g :: Double,
  b :: Double
}
instance Eq Color where
  (Color r1 g1 b1) == (Color r2 g2 b2) = r1 == r2 && g1 == g2 && b1 == b2
instance Show Color

type ColorMultiplier = Color

newtype ReflectionStrategy = Mirror {multiplier :: ColorMultiplier}

-- accepts an incident ray and normal vector, and returns a list of (reflected ray, multiplier)
hit :: ReflectionStrategy -> V3 Double -> V3 Double -> [(V3 Double, ColorMultiplier)]
hit (Mirror multiplier) incidentRay normalVector = [(
      V.reflect incidentRay normalVector,
      multiplier
    )]
