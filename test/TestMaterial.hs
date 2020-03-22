module TestMaterial
    ( unitTests
    ) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Linear.V3

import Material as M

unitTests = testGroup "Material unit tests" [
    mirrorTests
  ]

mirrorTests = testGroup "Mirror material" [
    testCase "Simple reflection" $ assertEqual ""
      [(V3 0 (-1) 1, 0.25)]
      (M.hit (M.Mirror 0.25) (V3 0 (-1) (-1)) (V3 0 0 1))
  ]
