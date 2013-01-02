{-# LANGUAGE DataKinds, TypeFamilies, TemplateHaskell, QuasiQuotes
  , TypeOperators, NoImplicitPrelude #-}

import Units.Prelude
import Units.Metric

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

-- Unit conversion

type Yard = [u|yd|]
makeUnit ''Yard

yards :: Fractional a => a :@ Yard -> a :@ Meter
yards = (*) (0.9144 * meter/yard)

toKMH :: Fractional a => a :@ Meter/Second -> a :@ Kilo*Meter/Hour
toKMH mps = mps * (3600 * second/hour) / (1000/kilo)

-- Test commution of km/h

testkmh :: (Kilo*Meter/Hour) ~ (Meter/Hour*Kilo)  => ()
testkmh = ()

-- Some testing of the sqrt stuff

{-
testplanck :: Volume ~ (Sqrt ((HBar*G)^3 / C^9)) => ()
testplanck = ()
-}

-- If this thing compiles, all of our tests have succeeded

main :: IO ()
main = return ()
