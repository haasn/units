{-# LANGUAGE ConstraintKinds, TypeFamilies, TypeOperators, DataKinds, PolyKinds
  , FlexibleInstances, UndecidableInstances, ScopedTypeVariables, GADTs #-}

module Units.Convert where

import Prelude hiding (Int)
import Data.Singletons
import GHC.Exts (Constraint)

import Units
import Units.Internal.Types

class IsoDim (u :: [TChar]) where
  type From u :: Unit

  factor :: Fractional a => p u -> a -- From u / u

type family Base (u :: Unit) :: Unit where
  Base (EL  '[]        ) = One
  Base (EL ((u:^e)':us)) = From u ^^ e * Base (EL us)

type family HasFactor (u :: Unit) :: Constraint where
  HasFactor (EL '[])              = ()
  HasFactor (EL ((u :^ e) ': xs)) = (HasFactor (EL xs), IsoDim u)

-- Convert between applicable units

type Convert u v = (Linear u, Linear v, Base u ~ Base v)

convert :: forall a u v. (Fractional a, Convert u v) => a :@ u -> a :@ v
convert (U n) = U (n * f1 / f2)
  where f1 = getFactor (Proxy :: Proxy u)
        f2 = getFactor (Proxy :: Proxy v)

normalize :: forall a u. (Fractional a, Linear u) => a :@ u -> a :@ Base u
normalize (U n) = U (n * f1)
  where f1 = getFactor (Proxy :: Proxy u)

class HasFactor u => Linear (u :: Unit) where
  getFactor :: Fractional a => p u -> a

instance Linear (EL '[]) where
  getFactor _ = 1

instance (SingRep e, IsoDim x, Linear (EL xs))
          => Linear (EL ((x :^ e) ': xs)) where
  getFactor _ = factor (Proxy :: Proxy x) ^^ fromSing (sing :: Sing e)
                  * getFactor (Proxy :: Proxy (EL xs))
