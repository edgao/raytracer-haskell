{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Config where

import qualified Data.Yaml as Y
import Data.Yaml ((.:))
import qualified Data.Vector as DV
import Data.Vector ((!))
import qualified Data.Aeson.Types as A
import qualified Control.Monad as M

import qualified Shape as S
import qualified Material as M
import Linear.V3 (V3 (..))

import Data.Char as C
import qualified Data.Aeson.TH as ATH
import qualified Control.Monad as M

-- TODO combine these somehow

instance Y.FromJSON (V3 Double) where
  parseJSON (Y.Object o) = V3 <$> (o .: "x") <*> (o .: "y") <*> (o .: "z")
  parseJSON (Y.Array a)
    | DV.length a == 3 = V3 <$> A.parseJSON (a ! 0) <*> A.parseJSON (a ! 1) <*> A.parseJSON (a ! 2)
    | otherwise = A.prependFailure "parsing vector failed, " (M.fail $ "Expected an array of length 3, got " ++ show a)
  parseJSON invalid = A.prependFailure "parsing vector failed, " (M.fail $ "Expected array of length 3 or dictionary; got " ++ show invalid)
instance Y.ToJSON (V3 Double) where
  toJSON (V3 x y z) = Y.array [Y.toJSON x, Y.toJSON y, Y.toJSON z]
  -- TODO?
  -- toEncoding (V3 x y z) = 

instance Y.FromJSON M.Color where
  parseJSON (Y.Object o) = M.Color <$> (o .: "r") <*> (o .: "g") <*> (o .: "b")
  parseJSON (Y.Array a)
    | DV.length a == 3 = M.Color <$> A.parseJSON (a ! 0) <*> A.parseJSON (a ! 1) <*> A.parseJSON (a ! 2)
    | otherwise = A.prependFailure "parsing color failed, " (M.fail $ "Expected an array of length 3, got " ++ show a)
  parseJSON invalid = A.prependFailure "parsing color failed, " (M.fail $ "Expected array of length 3 or dictionary; got " ++ show invalid)
instance Y.ToJSON M.Color where
  toJSON (M.Color r g b) = Y.array [Y.toJSON r, Y.toJSON g, Y.toJSON b]
  -- TODO?
  -- toEncoding (Color r g b) = 

$(ATH.deriveJSON ATH.defaultOptions{A.constructorTagModifier = map C.toLower} ''S.Shape)
$(ATH.deriveJSON ATH.defaultOptions{A.constructorTagModifier = map C.toLower} ''M.Light)
$(ATH.deriveJSON ATH.defaultOptions{A.constructorTagModifier = map C.toLower} ''M.Material)
$(ATH.deriveJSON ATH.defaultOptions{A.constructorTagModifier = map C.toLower} ''M.ReflectionStrategy)

data Object = Object {
  shape :: S.Shape,
  material :: M.Material
} deriving (Eq, Show)
$(ATH.deriveJSON ATH.defaultOptions{A.constructorTagModifier = map C.toLower} ''Object)

data World = World {
  objects :: [Object],
  lights :: [M.Light],
  ambientLight :: M.Color
} deriving (Eq, Show)
$(ATH.deriveJSON ATH.defaultOptions{A.constructorTagModifier = map C.toLower} ''World)

data Raytracer = Raytracer {
  reflectionStrategy :: M.ReflectionStrategy,
  maxBounces :: Int
} deriving (Eq, Show)
$(ATH.deriveJSON ATH.defaultOptions{A.constructorTagModifier = map C.toLower} ''Raytracer)

data Camera = Camera {
  position :: V3 Double,
  direction :: V3 Double
} deriving (Eq, Show)
$(ATH.deriveJSON ATH.defaultOptions{A.constructorTagModifier = map C.toLower} ''Camera)

data Viewport = Viewport {
  vertical :: V3 Double,
  width :: Double,
  height :: Double,
  distance :: Double
} deriving (Eq, Show)
$(ATH.deriveJSON ATH.defaultOptions{A.constructorTagModifier = map C.toLower} ''Viewport)

data Image = Image {
  width :: Int,
  height :: Int,
  camera :: Camera,
  viewport :: Viewport
} deriving (Eq, Show)
$(ATH.deriveJSON ATH.defaultOptions{A.constructorTagModifier = map C.toLower} ''Image)

data Config = Config {
  world :: World,
  raytracer :: Raytracer,
  image :: Image
} deriving (Eq, Show)
$(ATH.deriveJSON ATH.defaultOptions{A.constructorTagModifier = map C.toLower} ''Config)

toTuple :: Object -> (S.Shape, M.Material)
toTuple (Object shape material) = (shape, material)
