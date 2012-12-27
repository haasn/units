{-# LANGUAGE ConstraintKinds, TypeFamilies, TypeOperators, DataKinds, PolyKinds
  , FlexibleInstances, UndecidableInstances, ScopedTypeVariables, GADTs #-}

module Units.Convert where

import Prelude hiding (Int)
import Data.Singletons
import GHC.Exts (Constraint)

import Units
import Units.Internal.Types

-- We need a kind-polymorphic version
data Proxy k = Proxy

class IsoDim (u :: [TChar]) where
  type From u :: Unit

  factor :: Fractional a => p u -> a -- From u / u

type family Base (u :: Unit) :: Unit
type instance Base (EL  '[]        ) = One
type instance Base (EL ((u:^e)':us)) = From u ^^ e * Base (EL us)

type family Linear (u :: Unit) :: Constraint
type instance Linear (EL '[])              = ()
type instance Linear (EL ((u :^ e) ': xs)) = (Linear (EL xs), IsoDim u)

-- Convert between applicable units

type Convert u v = (Linear u, SingI u, Linear v, SingI v, Base u ~ Base v)

convert :: forall a u v. (Fractional a, Convert u v) => a :@ u -> a :@ v
convert (U n) = U (n * f1 / f2)
  where f1 = getFactor (sing :: Sing u)
        f2 = getFactor (sing :: Sing v)

getFactor :: (Fractional a, Linear u) => Sing u -> a
getFactor (SEL SNil) = 1
getFactor (SEL (SCons (u :%^ e) xs)) =
  factor u ^^ fromInt (fromSing e) * getFactor (SEL xs)

fromInt :: Int -> Integer
fromInt (Norm n) = fromNat n
fromInt (Neg  n) = (-1) - fromNat n

fromNat :: Nat -> Integer
fromNat N0     = 0
fromNat (NS n) = 1 + fromNat n
