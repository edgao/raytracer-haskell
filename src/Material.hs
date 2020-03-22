module Material
    ( Color (..),
      ReflectionStrategy (..),
      reflect
    ) where

import Linear.V3 (V3 (..))
import qualified Vector as V
import Vector ((.*.), (***), (+*+), (-*-))

-- 0 represents minimum brightness, 1 represents maximum.
-- Callers should use clipColor to sanitize values outside those bounds.
data Color = Color {
  r :: Double,
  g :: Double,
  b :: Double
}
instance Eq Color where
  (Color r1 g1 b1) == (Color r2 g2 b2) = r1 == r2 && g1 == g2 && b1 == b2
instance Show Color

(+^+) :: Color -> Color -> Color
(+^+) (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 + r2) (g1 + g2) (b1 + b2)

(*^*) :: ColorMultiplier -> Color -> Color
(*^*) (Color rm gm bm) (Color r g b) = Color (rm *r) (gm * g) (bm * b)

(*^^*) :: Double -> Color -> Color
(*^^*) c (Color r g b) = Color (c * r) (c * g) (c * b)

clipColor :: Color -> Color
clipColor (Color r g b) = Color (clip r) (clip g) (clip b)
  where clip x = min 1 $ max 0 x

type ColorMultiplier = Color

newtype ReflectionStrategy = Mirror {multiplier :: ColorMultiplier}

-- accepts an incident ray and normal vector, and returns a list of (reflected ray, multiplier)
reflect :: ReflectionStrategy -> V3 Double -> V3 Double -> [(V3 Double, ColorMultiplier)]
reflect (Mirror multiplier) incidentRay normalVector = [(
      V.reflect incidentRay normalVector,
      multiplier
    )]

data Material = PhongMaterial {
  ambient :: ColorMultiplier,
  diffuse :: ColorMultiplier,
  specular :: ColorMultiplier,
  shininess :: Double
}

reflectedLight :: Material -> [(Color, V3 Double)] -> Color -> V3 Double -> V3 Double -> Color
-- Light locations and viewer location are relative to the point of reflection
-- I.e. they are treated as local coordinates
reflectedLight (PhongMaterial ambientCoeff diffuseCoeff specularCoeff shininess) lights ambientLight normal viewer =
  let reflections = foldr ((+^+) . calculateLight) (Color 0 0 0) lights
      ambientReflection = ambientCoeff *^* ambientLight
  in reflections +^+ ambientReflection
  where calculateLight (lightColor, lightLocation) =
          let diffuse = diffuseCoeff *^* ((V.normalize lightLocation .*. normal) *^^* lightColor)
              reflectedLightDirection = V.negReflect (V.normalize lightLocation) normal
              specular = specularCoeff *^* (((reflectedLightDirection .*. V.normalize viewer) ** shininess) *^^* lightColor)
          in diffuse +^+ specular
