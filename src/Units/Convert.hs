{-# LANGUAGE ConstraintKinds, TypeFamilies, TypeOperators, DataKinds, PolyKinds
  , FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}

module Units.Convert where

import Prelude hiding (Int)
import Data.Singletons

import Units
import Units.Internal.Types

-- We need a kind-polymorphic version
data Proxy k = Proxy

class IsoDim (u :: [TChar]) where
  type From u :: [TChar]

  factor :: Fractional a => p u -> a -- From u / u

type family Base (u :: Unit) :: Unit
type instance Base (EL u) = EL (Cleanup (Merge (Sort (Strip (Base' u)))))

-- Take each primitive unit to its base form
type family Base' (u :: [Assoc]) :: [Assoc]
type instance Base' '[]          = '[]
type instance Base' ((u:^e)':us) = (From u :^ e) ': Base' us

-- Get rid of units with a dimensionless (empty) base
type family Strip (u :: [Assoc]) :: [Assoc]
type instance Strip '[] = '[]
type instance Strip (('[]      :^ e) ': us) = Strip us
type instance Strip (((n ':ns) :^ e) ': us) = ((n ':ns) :^ e) ': Strip us

-- Merge two adjacent units with the same base
type family Merge (u :: [Assoc]) :: [Assoc]
type instance Merge  '[ ]                = '[ ]
type instance Merge  '[x]                = '[x]
type instance Merge ((u:^e)':(v:^d)':us) =
  If (u :== v)
    ((u :^ AddInt e d)    ': Merge us)
    ((u :^ e) ': (v :^ d) ': Merge us)

-- Internal class for multiplying factors
class Linear (u :: Unit) where
  getFactor :: Fractional a => p u -> a

instance Linear (EL '[]) where
  getFactor _ = 1

instance (Linear (EL xs), GetInt e, IsoDim u)
         => Linear (EL ((u :^ e) ': xs)) where
  getFactor _ = factor (Proxy :: Proxy u) ^^ getInt (Proxy :: Proxy e)
                * getFactor (Proxy :: Proxy (EL xs))

-- Convert a type-Int to value level representation
class GetInt (u :: Int) where
  getInt :: p u -> Integer

instance GetNat n => GetInt (Norm n) where
  getInt _ = getNat (Proxy :: Proxy n)

instance GetNat n => GetInt (Neg n)
  where getInt _ = (-1) - getNat (Proxy :: Proxy n)

-- Convert a type-Nat to value level representation
class GetNat (u :: Nat) where
  getNat :: p u -> Integer

instance GetNat N0 where
  getNat _ = 0

instance GetNat n => GetNat (NS n) where
  getNat _ = 1 + getNat (Proxy :: Proxy n)

-- Convert between applicable units

type Convert u v = (Linear u, Linear v, Base u ~ Base v)

convert :: forall a u v. (Fractional a, Convert u v) => a :@ u -> a :@ v
convert (U n) = U (n * f1 / f2)
  where f1 = getFactor (Proxy :: Proxy u)
        f2 = getFactor (Proxy :: Proxy v)
