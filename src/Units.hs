{-# LANGUAGE KindSignatures, DataKinds, TemplateHaskell, TypeFamilies
  , UndecidableInstances, TypeOperators, PolyKinds, QuasiQuotes, RankNTypes
  , FlexibleContexts, GADTs #-}
module Units
  -- * Types
  ( (:@)(), One, Unit

  -- ** Type functions for combining units
  , (*)(), (/)(), (^)(), (^^)()

  -- * Type-safe calculations with units
  , addU, subU, mulU, divU, sqrtU
  , lit, unTag

  -- * Type-unsafe functions
  , coerceUnit
  ) where

import Prelude hiding (Int)

import Units.Internal.TypeOrd (Compare)
import Units.Internal.Types

import qualified GHC.TypeLits as GHC (Nat)

-- Merging by adding. Units are pre-sorted, so this preserves invariants

type family InsertAdd (x :: Assoc) (xs :: [Assoc]) :: [Assoc]
type instance where
  InsertAdd  x       '[]              = '[x]
  InsertAdd (x :^ e) ((y :^ f) ': ys) =
    InsertAdd' (Compare x y) (x :^ e) (y :^ f) ys

type family InsertAdd' (o :: Ordering) (x :: Assoc) (y :: Assoc) (ys :: [Assoc]) :: [Assoc]
type instance where
  InsertAdd' LT x y ys = x ': y ': ys
  InsertAdd' GT x y ys = y ': InsertAdd x ys
  InsertAdd' EQ (x :^ Norm (NS e)) (y :^ Neg e) ys = ys -- Delete 0s
  InsertAdd' EQ (x :^ Neg e) (y :^ Norm (NS e)) ys = ys -- Delete 0s
  InsertAdd' EQ (x :^ e) (y :^ f) ys = (x :^ (e+f)) ': ys

-- Common operators: Multiplication, Exponentiation, Division

type instance where
  EL xs * EL '[]       = EL xs
  EL xs * EL (y ': ys) = EL (InsertAdd y xs) * EL ys

type family MapMul (f :: Int) (u :: [Assoc]) :: [Assoc]
type instance where
  MapMul f '[]              = '[]
  MapMul f ((x :^ e) ': xs) = (x :^ (f*e)) ': MapMul f xs

-- | Exponentiate a unit to a Units.Int you have lying around.

type family (u :: Unit) ^^ (i :: Int) :: Unit
type instance (EL xs) ^^ i = EL (MapMul i xs)
infixr 8 ^^

-- | Exponentiate a unit to a natural exponent. This only works with exponents
--   from 0 to 9, due to limitations in GHC's Nat kind.
--
--   > a ^ 0 ~ One
--   > a ^ 1 ~ a

type family (u :: Unit) ^ (n :: GHC.Nat) :: Unit
type instance u ^ n = u ^^ IntLit n
infixr 8 ^

type family Recip (u :: Unit) :: Unit
type instance Recip xs = xs ^^ IM1

type instance where
  xs / ys = xs * Recip ys

-- | The dimensionless unit. This is the multiplicative identity of units.

type One = EL '[]

{-
Where to stick this documentation?

-- | Multiply two units. This has commutative, associative and has 'One' as
--   the identity:
--
--   > a * b ~ b * a
--   > (a * b) * c ~ a * (b * c)
--   > One * a ~ a
--   > a * One ~ a

-- | Divide two units. This is equal to multiplying with the reciprocal of
--   the right unit.
-}

-- Type-safe unit calculations

-- | Add two numbers with units. The units have to align for this to work.

addU :: Num a => a :@ u -> a :@ u -> a :@ u
addU (U a) (U b) = U (a+b)

-- | Subtract two numbers with units. As with addition, the units have to
--   be identical.

subU :: Num a => a :@ u -> a :@ u -> a :@ u
subU (U a) (U b) = U (a-b)

-- | Multiply two numbers with units, multiplying their units in the process.

mulU :: Num a => a :@ u -> a :@ v -> a :@ u*v
mulU (U a) (U b) = U (a*b)

-- | Divide two fractionals with units, dividing their units in the process.

divU :: Fractional a => a :@ u -> a :@ v -> a :@ u/v
divU (U a) (U b) = U (a/b)

-- | Take the square root of a squared unit
sqrtU :: Floating a => a :@ u^2 -> a :@ u
sqrtU (U a) = U (sqrt a)

-- | Project any value into a dimensionless quantity

lit :: a -> a :@ One
lit = U

-- | Untag a dimensionless quantity

unTag :: a :@ One -> a
unTag (U a) = a

-- Type-unsafe unit calculations

-- | Coerce the units while leaving the value unchanged

coerceUnit :: a:@u -> a:@v
coerceUnit (U a) = U a
