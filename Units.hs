{-# LANGUAGE KindSignatures, DataKinds, TemplateHaskell, TypeFamilies
  , UndecidableInstances, TypeOperators, PolyKinds, QuasiQuotes #-}
module Units
  ( (:@)()
  , (*)(), (/)(), (^)(), (^^)(), (%)(), Sqrt
  , One
  , addU, subU, mulU, divU
  , lit, unU, coerceUnit
  ) where

import Prelude hiding (Int, div, Rational)
import Data.Singletons
import Units.Types

import qualified GHC.TypeLits as GHC (Nat)

promote [d|
  -- Equality checking

  eqAList :: [Assoc] -> [Assoc] -> Bool
  eqAList  []          []    = True
  eqAList  []         (x:xs) = False
  eqAList (x:xs)       []    = False
  eqAList ((s:^e):xs) (y:ys) = eqAList' e (extract s (y:ys))

  eqAList' :: Rational -> (Maybe Rational, [Assoc]) -> Bool
  eqAList' e (Just e', _) = e == e'
  eqAList' _ (Nothing, _) = False

  eqUnit :: Unit -> Unit -> Bool
  eqUnit (EL a) (EL b) = eqAList a b

  -- Lookup

  extract :: [TChar] -> [Assoc] -> (Maybe Rational, [Assoc])
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

  insertAdd' :: Assoc -> (Maybe Rational, [Assoc]) -> [Assoc]
  insertAdd' v (Nothing, x)      = v:x
  insertAdd' (s:^e) (Just e', x) = (s :^ addRat e e') : x

  -- Merging

  mergeAdd :: [Assoc] -> [Assoc] -> [Assoc]
  mergeAdd  []   y = y
  mergeAdd (v:x) y = insertAdd v (mergeAdd x y)

  multUnit :: Unit -> Unit -> Unit
  multUnit (EL a) (EL b) = EL (cleanup (mergeAdd a b))

  -- Multiplication with constant factor

  mapMul :: Rational -> [Assoc] -> [Assoc]
  mapMul _  []        = []
  mapMul r ((s:^e):x) = (s :^ mulRat r e) : mapMul r x

  recip :: Unit -> Unit
  recip (EL a) = EL (mapMul rm1 a)

  powUnit :: Unit -> Rational -> Unit
  powUnit (EL a) r = EL (mapMul r a)

  -- Cleanup of 0s

  cleanup :: [Assoc] -> [Assoc]
  cleanup []         = []
  cleanup ((s:^e):x) = if e == r0 then x else (s:^e) : cleanup x
  |]

type instance (a :: Unit) :==: (b :: Unit) = EqUnit a b

-- Pretty operators for combining types

type family (a :: Unit) * (b :: Unit) :: Unit
type instance a*b = MultUnit a b
infixl 7 *

type family (a :: Unit) / (b :: Unit) :: Unit
type instance a/b = MultUnit a (Recip b)
infixl 7 /

type family (a :: Unit) ^ (b :: GHC.Nat) :: Unit
type instance a^b = PowUnit a (IntLit b :/ I1)
infixr 8 ^

type family (a :: Unit) ^^ (b :: Rational) :: Unit
type instance a^^b = PowUnit a b
infixr 8 ^^

type family (a :: GHC.Nat) % (b :: GHC.Nat) :: Rational
type instance a%b = MkRatio (IntLit a) (IntLit b)
infix 9 %

type family Sqrt (a :: Unit) :: Unit
type instance Sqrt a = a ^^ (1%2)

-- Type-safe unit calculations

addU :: (Num a, (u :==: v) ~ True) => a:@u -> a:@v -> a:@u
addU (U a) (U b) = U (a+b)

subU :: (Num a, (u :==: v) ~ True) => a:@u -> a:@v -> a:@u
subU (U a) (U b) = U (a-b)

mulU :: Num a => a:@u -> a:@v -> a:@(u*v)
mulU (U a) (U b) = U (a*b)

divU :: Fractional a => a:@u -> a:@v -> a:@(u/v)
divU (U a) (U b) = U (a/b)

lit :: a -> a :@ One
lit = U

unU :: a :@ One -> a
unU (U a) = a

-- Type-unsafe unit calculations

coerceUnit :: a:@u -> a:@v
coerceUnit (U a) = U a
