module TestMaterial
    ( unitTests
    ) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Linear.V3 (V3 (..))

import Material as M

unitTests = testGroup "Material" [
    mirrorTests
  ]

mirrorTests = testGroup "Mirror" [
    testCase "Simple reflection" $ assertEqual ""
      [(V3 0 (-1) 1)]
      (M.reflect M.Mirror (V3 0 (-1) (-1)) (V3 0 0 1))
  ]
