{-# LANGUAGE KindSignatures, DataKinds, TemplateHaskell, TypeFamilies
  , UndecidableInstances, TypeOperators, PolyKinds, QuasiQuotes, RankNTypes
  , FlexibleContexts, GADTs #-}
module Units
  -- * Types
  ( (:@)(), One, Unit, U

  -- ** Type functions for combining units
  , (*)(), (/)(), (^)(), (^^)()

  -- * Type-safe calculations with units
  , addU, subU, mulU, divU, sqrtU
  , lit, unTag

  -- * Type-unsafe functions
  , coerceUnit
  ) where

import Prelude hiding (Int)

import Units.Internal.Types

import GHC.TypeLits (Symbol, CmpSymbol)
import qualified GHC.TypeLits as GHC (Nat)

-- Merging by adding. Units are pre-sorted, so this preserves invariants

type family Merge (xs :: [Assoc]) (ys :: [Assoc]) :: [Assoc] where
  Merge xs '[] = xs
  Merge '[] ys = ys
  Merge ((x :^ e) ': xs) ((y :^ f) ': ys) = Merge' (CmpSymbol x y) (x :^ e) (y :^ f) xs ys

type family Merge' (o :: Ordering) (x :: Assoc) (y :: Assoc) (xs :: [Assoc]) (ys :: [Assoc]) :: [Assoc] where
  Merge' LT x y xs ys = x ': Merge xs (y ': ys)
  Merge' GT x y xs ys = y ': Merge (x ': xs) ys
  Merge' EQ (x :^ e) (y :^ f) xs ys = DelZ x (e+f) (Merge xs ys)

type family DelZ (x :: Symbol) (e :: Int) (xs :: [Assoc]) :: [Assoc] where
  DelZ x (Norm 0) xs = xs
  DelZ x e        xs = (x :^ e) ': xs

-- Common operators: Multiplication, Exponentiation, Division

type instance EL xs * EL ys = EL (Merge xs ys)

type family MapMul (f :: Int) (u :: [Assoc]) :: [Assoc] where
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
type instance u ^ n = u ^^ Norm n
infixr 8 ^

type family Recip (u :: Unit) :: Unit
type instance Recip xs = xs ^^ Neg 0

type instance xs / ys = xs * Recip ys

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

-- | Base unit for a named type

type U s = EL '[ s :^ Norm 1 ]
