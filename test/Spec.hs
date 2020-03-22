import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import qualified TestVector as V
import qualified TestMaterial as M

main = defaultMain $ testGroup "Unit tests" [
    V.unitTests,
    M.unitTests
  ]
