{-# LANGUAGE ConstraintKinds, TypeFamilies, TypeOperators, DataKinds, PolyKinds
  , FlexibleInstances, UndecidableInstances, ScopedTypeVariables, GADTs #-}

module Units.Convert where

import Prelude hiding (Int)
import GHC.Exts (Constraint)

import Units
import Units.Internal.Types

-- Kind-polymorphic proxy
data Proxy k = Proxy

class IsoDim (u :: [TChar]) where
  type From u :: Unit

  factor :: Fractional a => p u -> a -- From u / u

type family Base (u :: Unit) :: Unit
type instance where
  Base (EL  '[]        ) = One
  Base (EL ((u:^e)':us)) = From u ^^ e * Base (EL us)

type family HasFactor (u :: Unit) :: Constraint
type instance where
  HasFactor (EL '[])              = ()
  HasFactor (EL ((u :^ e) ': xs)) = (HasFactor (EL xs), IsoDim u)

-- Convert between applicable units

type Convert u v = (Linear u, Linear v, Base u ~ Base v)

convert :: forall a u v. (Fractional a, Convert u v) => a :@ u -> a :@ v
convert (U n) = U (n * f1 / f2)
  where f1 = getFactor (Proxy :: Proxy u)
        f2 = getFactor (Proxy :: Proxy v)

-- Reflection classes, less overhead than constructing Sing instances

class ReflNat (n :: Nat) where
  reflNat :: p n -> Integer

instance ReflNat N0 where
  reflNat _ = 0

instance ReflNat n => ReflNat (NS n) where
  reflNat _ = 1 + reflNat (Proxy :: Proxy n)


class ReflInt (n :: Int) where
  reflInt :: p n -> Integer

instance ReflNat n => ReflInt (Norm n) where
  reflInt _ = reflNat (Proxy :: Proxy n)

instance ReflNat n => ReflInt (Neg n) where
  reflInt _ = (-1) - reflNat (Proxy :: Proxy n)


class HasFactor u => Linear (u :: Unit) where
  getFactor :: Fractional a => p u -> a

instance Linear (EL '[]) where
  getFactor _ = 1

instance (ReflInt e, IsoDim x, Linear (EL xs))
          => Linear (EL ((x :^ e) ': xs)) where
  getFactor _ = factor (Proxy :: Proxy x) ^^ reflInt (Proxy :: Proxy e)
                  * getFactor (Proxy :: Proxy (EL xs))
