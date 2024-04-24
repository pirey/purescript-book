module Test.MySolutions where

import Data.Int (rem)
import Data.Number (pi, sqrt)
import Prelude ((*), (+))

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea r = pi * (r * r)

leftoverCents :: Int -> Int
leftoverCents n = rem n 100

