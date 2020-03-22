module TestVector
    ( unitTests
    ) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestUtil (assertVecEqual)

import Linear.V3
import Vector as V

unitTests = testGroup
  "Vector unit tests"
  [
    testCase "Dot product" $ assertEqual "" 32 (V.dot (V3 1 2 3) (V3 4 5 6)),
    testCase "Scale" $ assertEqual "" (V3 2 4 6) (V.scale 2 (V3 1 2 3)),
    testCase "Add" $ assertEqual "" (V3 5 7 9) (V.add (V3 1 2 3) (V3 4 5 6)),
    testCase "Norm" $ assertEqual "" 7 (V.norm (V3 2 3 6)),
    testCase "Normalize" $ assertVecEqual ""  0.0001 (V3 (2/7) (3/7) (6/7)) (V.normalize (V3 2 3 6)),
    testCase "Simple reflection" $ assertVecEqual "" 0.0001
      (V3 0.57735 0.57735 0.57735)
      (V.reflect (V.normalize (V3 1 1 (-1))) (V3 0 0 1)),
    testCase "Angled reflection 1" $ assertVecEqual "" 0.0001
      (V3 0.2357 0.2357  0.942809)
      (V.reflect (V.normalize (V3 (-1) (-1) 0)) (V.normalize (V3 1 1 1))),
    testCase "Angled reflection 2" $ assertVecEqual "" 0.0001
      (V3 0.4444 0.4444 0.7777)
      (V.reflect (V.normalize (V3 (-2) (-2) (-1))) (V.normalize (V3 1 1 1)))
  ]
