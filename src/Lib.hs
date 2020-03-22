module Lib
    ( someFunc
    ) where

import Linear.V3
import qualified Vector as V

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Shape = Triangle {a :: V3 Double, b :: V3 Double, c :: V3 Double}
           | Ellipsoid {center :: V3 Double, r1 :: V3 Double, r2 :: V3 Double, r3 :: V3 Double}
