{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Material where

import Linear.V3 (V3 (..))
import qualified Linear.V3 as V3
import qualified Vector as V
import Vector ((.*.), (***), (+*+), (-*-))

import qualified Control.Applicative as A

-- 0 represents minimum brightness, 1 represents maximum.
-- Callers should use clampColor to sanitize values outside those bounds.
data Color = Color {
  r :: Double,
  g :: Double,
  b :: Double
} deriving (Eq, Show)

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

data ReflectionStrategy = Mirror
  | SphericalBurst {
    n :: Int
  } deriving (Eq, Show)

-- accepts an incident ray and normal vector, and returns a list of (reflected ray, multiplier)
reflect :: ReflectionStrategy -> V3 Double -> V3 Double -> [V3 Double]
reflect Mirror incidentRay normalVector = [V.reflect incidentRay normalVector]
reflect (SphericalBurst n) incidentRay normalVector = burstRays
  where reflectedRay = V.normalize $ V.reflect incidentRay normalVector
        axis = V.reject normalVector reflectedRay
        angles = map getAngle [0,1..n]
          where getAngle x = 2 * fromIntegral x * pi / (fromIntegral n + 1)
        burstRays = filter (\ray -> ray .*. normalVector > 0) $ map getRay $ A.liftA2 (,) angles angles
          where getRay (a1, a2) = V.rotate (V.rotate reflectedRay axis a1) reflectedRay a2

data Material = PhongMaterial {
  ambient :: ColorMultiplier,
  diffuse :: ColorMultiplier,
  specular :: ColorMultiplier,
  shininess :: Double
} deriving (Eq, Show)

data Light = Light {
  intensity :: Color,
  location :: V3 Double
} deriving (Eq, Show)

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
