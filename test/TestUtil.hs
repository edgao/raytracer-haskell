module TestUtil
    ( assertVecEqual
    ) where

import Test.Tasty.HUnit (Assertion, assertFailure)
import Control.Monad (unless)

import Linear.V3

-- A slightly tweaked version of assertEquals, which accepts a margin of error (to handle rounding error)
assertVecEqual :: String -> Double -> V3 Double -> V3 Double -> Assertion
assertVecEqual preface threshold expected@(V3 ex ey ez) actual@(V3 ax ay az) = 
  unless (abs(ex - ax) < threshold && abs(ey - ay) < threshold && abs(ez - az) < threshold) (assertFailure msg)
  where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ show expected ++ "\n but got: " ++ show actual
