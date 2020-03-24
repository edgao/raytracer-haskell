module Main where

import qualified Data.ByteString.Lazy as BS (writeFile)
import Codec.Picture as P (PixelRGB8 (..), DynamicImage (ImageRGB8), generateImage)
import Codec.Picture.Saving as PS (imageToPng)

import qualified Material as M
import Linear.V3 (V3 (..))
import qualified Vector as V

main :: IO ()
main = do
  let width = 1000
  let height = 1000
  let 
    fn :: Int -> Int -> PixelRGB8
    fn imgX imgY = 
        let x :: Double
            x = 4.0 * (fromIntegral imgX) / (fromIntegral width) - 2
            y :: Double
            y = 2 - 4.0 * (fromIntegral imgY) / (fromIntegral height)
            intersection = sphereIntersection x y
        in case intersection of
            Nothing -> P.PixelRGB8 0 0 0
            Just location -> P.PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)
              where (r, g, b) = M.render $ M.clampColor $ M.reflectedLight material lights ambientLight (V.normalize location) (V3 x y 10) location
  let img = P.generateImage fn width height
  BS.writeFile "/home/edgao/Desktop/test.png" $ PS.imageToPng $ P.ImageRGB8 img
  return ()

sphereIntersection :: Double -> Double -> Maybe (V3 Double)
sphereIntersection x y = if x * x + y * y > 1 then Nothing
  else Just $ V3 x y (sqrt (1 - x * x - y * y))

material = M.PhongMaterial (M.Color 0.2 0 0) (M.Color 0 0.7 0) (M.Color 0 0 1) 10

lights = [
    (
      M.Light
      (M.Color 1 1 1)
      (V3 100 100 100)
    )
  ]

ambientLight = M.Color 1 1 1
