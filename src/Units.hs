{-# LANGUAGE KindSignatures, DataKinds, TemplateHaskell, TypeFamilies
  , UndecidableInstances, TypeOperators, PolyKinds, QuasiQuotes #-}
module Units
  -- * Types
  ( (:@)(), One

  -- ** Type functions for combining units
  , (*)(), (/)(), (^)() --, (^^)(), (%)(), Sqrt

  -- * Type-safe calculations with units
  , addU, subU, mulU, divU
  , lit, unTag

  {-
  -- * Type-unsafe functions
  , coerceUnit
  -}

  -- Exported for internal purposes
  , Cleanup, Sort, Normalize
  ) where

import Prelude hiding (Int, div)
import Control.Applicative ((<|>))
import Control.Monad (liftM2)
import Data.Singletons

import Units.Internal.Types
--import Units.TH

import qualified GHC.TypeLits as GHC (Nat)

promote [d|
  -- Lookup

  extract :: [TChar] -> [Assoc] -> (Maybe Int, [Assoc])
  extract _  [] = (Nothing, [])
  extract s ((s':^e):xs) =
    if s == s'
      then (Just e, xs)
      else consSnd (s':^e) (extract s xs)

  consSnd :: x -> (b, [x]) -> (b, [x])
  consSnd x (b, xs) = (b, x:xs)

  -- Insertion

  insertAdd :: Assoc -> [Assoc] -> [Assoc]
  insertAdd (s:^e) x = insertAdd' (s:^e) (extract s x)

  insertAdd' :: Assoc -> (Maybe Int, [Assoc]) -> [Assoc]
  insertAdd' v (Nothing, x)      = v:x
  insertAdd' (s:^e) (Just e', x) = (s :^ addInt e e') : x

  -- Merging

  mergeAdd :: [Assoc] -> [Assoc] -> [Assoc]
  mergeAdd  []   y = y
  mergeAdd (v:x) y = insertAdd v (mergeAdd x y)

  multUnit :: Unit -> Unit -> Unit
  multUnit (EL a) (EL b) = EL (normalize (mergeAdd a b))

  -- Multiplication with constant factor

  mapMul :: Int -> [Assoc] -> [Assoc]
  mapMul _  []        = []
  mapMul r ((s:^e):x) = (s :^ mulInt r e) : mapMul r x

  recip :: Unit -> Unit
  recip (EL a) = EL (mapMul im1 a)

  powUnit :: Unit -> Int -> Unit
  powUnit (EL a) r = EL (mapMul r a)

  -- Cleanup of 0s and sorting

  cleanup :: [Assoc] -> [Assoc]
  cleanup []         = []
  cleanup ((s:^e):x) = if e == i0 then x else (s:^e) : cleanup x

  normalize :: [Assoc] -> [Assoc]
  normalize xs = sort (cleanup xs)

  |]

-- | The dimensionless unit. This is the multiplicative identity of units.

type One = EL '[]

-- Pretty operators for combining types

-- | Multiply two units. This has commutative, associative and has 'One' as
--   the identity:
--
--   > a * b ~ b * a
--   > (a * b) * c ~ a * (b * c)
--   > One * a ~ a
--   > a * One ~ a

type family (a :: Unit) * (b :: Unit) :: Unit
type instance a*b = MultUnit a b
infixl 7 *

-- | Divide two units. This is equal to multiplying with the reciprocal of
--   the right unit.

type family (a :: Unit) / (b :: Unit) :: Unit
type instance a/b = MultUnit a (Recip b)
infixl 7 /

-- | Exponentiate a unit to a natural exponent. This only works with exponents
--   from 0 to 9, due to limitations in GHC's Nat kind.
--
--   > a ^ 0 ~ One
--   > a ^ 1 ~ a

type family (a :: Unit) ^ (b :: GHC.Nat) :: Unit
type instance a^b = PowUnit a (IntLit b)
infixr 8 ^

-- | Exponentiate a unit to a rational exponent.
--
--   > a ^^ 0%1 ~ One
--   > a ^^ 1%1 ~ a

type family (a :: Unit) ^^ (b :: Int) :: Unit
type instance a^^b = PowUnit a b
infixr 8 ^^

{-
-- | Construct a rational number from two natural numbers. This only works with
--   arguments from 0 to 9, due to limitations in GHC's Nat kind.
--
--   Dividing by 0 will cause type-checking to fail to terminate.

type family (a :: GHC.Nat) % (b :: GHC.Nat) :: Rational
type instance a%b = MkRatio (IntLit a) (IntLit b)
infix 9 %

-- | Take the square root of a unit.
--
--   > Sqrt a ~ a ^^ 1%2
type family Sqrt (a :: Unit) :: Unit
type instance Sqrt a = a ^^ (1%2)
-}

-- Type-safe unit calculations

-- | Add two numbers with units. The units have to align for this to work.

addU :: Num a => a :@ u -> a :@ u -> a :@ u
addU (U a m) (U b n) = U (a+b) (m <|> n)

-- | Subtract two numbers with units. As with addition, the units have to
--   be identical.

subU :: Num a => a :@ u -> a :@ u -> a :@ u
subU (U a m) (U b n) = U (a-b) (m <|> n)

-- | Multiply two numbers with units, multiplying their units in the process.

mulU :: Num a => a :@ u -> a :@ v -> a :@ u*v
mulU (U a m) (U b n) = U (a*b) (liftM2 (*) m n)

-- | Divide two fractionals with units, dividing their units in the process.

divU :: Fractional a => a :@ u -> a :@ v -> a :@ u/v
divU (U a m) (U b n) = U (a/b) (liftM2 (/) m n)

-- | Project any number into a dimensionless quantity

lit :: Num a => a -> a :@ One
lit n = U n (Just 1)

-- | Untag a dimensionless quantity

unTag :: a :@ One -> a
unTag (U a _) = a

{-
-- Type-unsafe unit calculations

-- | Coerce the units while leaving the value unchanged

coerceUnit :: a:@u -> a:@v
coerceUnit (U a _) = U a Nothing
-}
