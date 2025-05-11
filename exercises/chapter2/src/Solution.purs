module Solution where

import Prelude

import Data.Int (rem)
import Data.Number (sqrt, pi)

diagonal s1 s2 = sqrt (s1 * s1 + s2 * s2)

circleArea r = pi * r * r

leftoverCents c = rem c 100
