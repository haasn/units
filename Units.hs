{-# LANGUAGE KindSignatures, DataKinds, TemplateHaskell, TypeFamilies
  , UndecidableInstances, TypeOperators, PolyKinds, QuasiQuotes #-}
module Units
  ( (:@)()
  , (*)(), (/)(), (^)()
  , One
  , addU, subU, mulU, divU
  , lit, coerceUnit
  ) where

import Prelude hiding (Int, div)
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

  eqAList' :: Int -> (Maybe Int, [Assoc]) -> Bool
  eqAList' e (Just e', _) = e == e'
  eqAList' _ (Nothing, _) = False

  eqUnit :: Unit -> Unit -> Bool
  eqUnit (EL a) (EL b) = eqAList a b

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
  multUnit (EL a) (EL b) = EL (cleanup (mergeAdd a b))

  -- Multiplication with constant factor

  mapMul :: Int -> [Assoc] -> [Assoc]
  mapMul _  []        = []
  mapMul i ((s:^e):x) = (s :^ mulInt i e) : mapMul i x

  recip :: Unit -> Unit
  recip (EL a) = EL (mapMul im1 a)

  powUnit :: Unit -> Int -> Unit
  powUnit (EL a) i = EL (mapMul i a)

  -- Cleanup of 0s

  cleanup :: [Assoc] -> [Assoc]
  cleanup []         = []
  cleanup ((s:^e):x) = if e == i0 then x else (s:^e) : cleanup x
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
type instance a^b = PowUnit a (IntLit b)
infixr 8 ^


-- Type-safe unit calculations

addU :: (Num a, (u :==: v) ~ True) => a:@u -> a:@v -> a:@u
addU (U a) (U b) = U (a+b)

subU :: (Num a, (u :==: v) ~ True) => a:@u -> a:@v -> a:@u
subU (U a) (U b) = U (a-b)

mulU :: Num a => a:@u -> a:@v -> a:@(u*v)
mulU (U a) (U b) = U (a*b)

divU :: Fractional a => a:@u -> a:@v -> a:@(u/v)
divU (U a) (U b) = U (a/b)

lit :: a -> a:@One
lit = U

-- Type-unsafe unit calculations

coerceUnit :: a:@u -> a:@v
coerceUnit (U a) = U a
