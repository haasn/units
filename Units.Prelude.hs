{-# LANGUAGE FlexibleInstances, TypeFamilies, TypeOperators, DataKinds #-}
module Units.Prelude ((+), (-), (*), (/), n) where
import Prelude hiding ((+), (-), (*), (/))

import Data.Singletons
import Units

(+) :: (Num a, (u :==: v) ~ True) => a:@u -> a:@v -> a:@u
(+) = addU
infixl 6 +

(-) :: (Num a, (u :==: v) ~ True) => a:@u -> a:@v -> a:@u
(-) = subU
infixl 6 -

(*) :: Num a => a:@u -> a:@v -> a:@(u*v)
(*) = mulU
infixl 7 *

(/) :: Fractional a => a:@u -> a:@v -> a:@(u/v)
(/) = divU
infixl 7 /

n :: a -> a :@ One
n = lit
