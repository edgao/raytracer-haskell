module Material
    ( Color (..),
      (+^+),
      (*^*),
      clampColor,
      render,
      ReflectionStrategy (..),
      reflect,
      Material (..),
      Light (..),
      reflectedLight
    ) where

import Linear.V3 (V3 (..))
import qualified Vector as V
import Vector ((.*.), (***), (+*+), (-*-))

-- 0 represents minimum brightness, 1 represents maximum.
-- Callers should use clampColor to sanitize values outside those bounds.
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

clampColor :: Color -> Color
clampColor (Color r g b) = Color (clamp r) (clamp g) (clamp b)
  where clamp x = min 1 $ max 0 x

render :: Color -> (Int, Int, Int)
render (Color r g b) = (renderChannel r, renderChannel g, renderChannel b)
  where renderChannel x = round (x * 255)

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

data Light = Light {
  intensity :: Color,
  location :: V3 Double
}

reflectedLight :: Material -> [Light] -> Color -> V3 Double -> V3 Double -> V3 Double -> Color
-- All location vectors (lights, viewer, reflectionPoint) are global coordinates
-- Returns a non-negative Color
reflectedLight (PhongMaterial ambientCoeff diffuseCoeff specularCoeff shininess) lights ambientLight normal viewer reflectionPoint =
  let reflections = foldr ((+^+) . calculateLight) (Color 0 0 0) lights
      ambientReflection = ambientCoeff *^* ambientLight
  in reflections +^+ ambientReflection
  where calculateLight (Light intensity lightLocation) =
          let lightDirection = V.normalize $ lightLocation - reflectionPoint
              diffuseProjection = lightDirection .*. normal
              diffuse = if diffuseProjection > 0 then diffuseCoeff *^* (diffuseProjection *^^* intensity)
                        else Color 0 0 0
              reflectedLightDirection = V.negReflect lightDirection normal
              viewerDirection = V.normalize $ viewer -*- reflectionPoint
              specularProjection = reflectedLightDirection .*. viewerDirection
              specular = if diffuseProjection > 0 && specularProjection > 0
                            then specularCoeff *^* ((specularProjection ** shininess) *^^* intensity)
                         else Color 0 0 0
          in diffuse +^+ specular
