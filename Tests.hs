{-# LANGUAGE TypeFamilies #-}
module Tests where
import Prelude hiding ((+), (-), (*), (/))

import Data.Singletons

import Units
import Units.Prelude
import Units.TH
import Units.SI

-- Tests

test1 :: Double :@ Meter
test1 = n 1*meter + n 2*meter

test2 :: Double :@ Second
test2 = n 5*second

test3 :: ([u|foo*bar|] :==: [u|bar*foo|]) ~ True => ()
test3 = ()

test4 :: Double :@ (Meter*Second)
test4 = test1 * test2

test5 :: (((Meter*Meter) :==: [u|m²|]) ~ True) => ()
test5 = ()

test6 :: Double :@ (Meter/Second)
test6 = test4 / (test2 * test2)

test7 :: Double :@ Kilogram
test7 = n 13.24*kilogram

type Newton = [u| kg*m/s² |]

test8 :: Double :@ Newton
test8 = test7 * test6 / test2
