{-# LANGUAGE DataKinds, TypeFamilies, TemplateHaskell, QuasiQuotes
  , TypeOperators #-}
module Units.Internal.Tests where
import Prelude hiding ((+), (-), (*), (/))

import Units
import Units.Prelude
import Units.Metric
import Units.Planck

-- Tests

test1 :: Double :@ Meter
test1 = 1*meter + 2*meter

test2 :: Double :@ Second
test2 = 5*second

test4 :: Double :@ Meter*Second
test4 = test1 * test2

test6 :: Double :@ Meter/Second
test6 = test4 / (test2 * test2)

test7 :: Double :@ Kilogram
test7 = 13.24*kilogram

test8 :: Double :@ Newton
test8 = test7 * test6 / test2

-- Example of how to convert units

type Yard = [u|yd|]
makeUnit ''Yard

yards :: Fractional a => a :@ Yard -> a :@ Meter
yards = (*) (0.9144 * meter/yard)

-- Example of unit-like literals

toKMH :: Fractional a => a :@ Meter/Second -> a :@ Kilo*Meter/Hour
toKMH mps = mps * (3600 * second/hour) / (1000/kilo)

testkmh :: (Kilo*Meter/Hour) ~ (Meter/Hour*Kilo)  => ()
testkmh = ()

-- Some testing of the sqrt stuff

testplanck :: Volume ~ (Sqrt ((HBar*G)^3 / C^9)) => ()
testplanck = ()
