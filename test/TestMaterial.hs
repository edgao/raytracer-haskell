module TestMaterial
    ( unitTests
    ) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Linear.V3
import Material as M

unitTests = testGroup
  "Material unit tests"
  [
    -- testCase "Dot product" $ assertEqual [] 32 (V.dot (V3 1 2 3) (V3 4 5 6))
  ]

mirrorTests = testGroup
  "Mirror material"
  []

-- simpleMirrorTest = testCase "Simple reflection" $ assertEqual [] (V3 -1 -1 1) ()
