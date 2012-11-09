{-# LANGUAGE DataKinds, TypeFamilies, TemplateHaskell, QuasiQuotes
  , TypeOperators #-}
module Tests where
import Prelude hiding ((+), (-), (*), (/))

import Data.Singletons

import Units
import Units.Prelude
import Units.TH
import Units.SI

-- Tests

test1 :: Double :@ Meter
test1 = 1*meter + 2*meter

test2 :: Double :@ Second
test2 = 5*second

test3 :: ([u|foo*bar|] :==: [u|bar*foo|]) ~ True => ()
test3 = ()

test4 :: Double :@ Meter*Second
test4 = test1 * test2

test5 :: (((Meter*Meter) :==: [u|m²|]) ~ True) => ()
test5 = ()

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

-- Test num injection

instance (Num a, u ~ One) => Num (a :@ u) where
  fromInteger = n . fromInteger

instance (Fractional a, u ~ One) => Fractional (a :@ u) where
  fromRational = n . fromRational

test9 :: Double :@ Newton
test9 = 1*newton
