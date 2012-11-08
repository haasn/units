{-# LANGUAGE KindSignatures, DataKinds, TemplateHaskell, TypeFamilies
  , UndecidableInstances, TypeOperators, PolyKinds, QuasiQuotes #-}
module Units where

import Prelude hiding (Int, div)

import Data.Singletons
import Units.Types
import Units.TH

promote [d|
  -- Equality checking

  eqExpList :: [Exp] -> [Exp] -> Bool
  eqExpList  []          []    = True
  eqExpList  []         (x:xs) = False
  eqExpList (x:xs)       []    = False
  eqExpList ((s:^e):xs) (y:ys) = eqExpList' e (extract s (y:ys))

  eqExpList' :: Int -> (Maybe Int, [Exp]) -> Bool
  eqExpList' e (Just e', _) = e == e'
  eqExpList' _ (Nothing, _) = False

  eqUnit :: Unit -> Unit -> Bool
  eqUnit (EL a) (EL b) = eqExpList a b

  -- Lookup

  extract :: [TChar] -> [Exp] -> (Maybe Int, [Exp])
  extract _  [] = (Nothing, [])
  extract s ((s':^e):xs) =
    if s == s'
      then (Just e, xs)
      else consSnd (s':^e) (extract s xs)

  consSnd :: x -> (b, [x]) -> (b, [x])
  consSnd x (b, xs) = (b, x:xs)

  -- Insertion

  insertAdd :: Exp -> [Exp] -> [Exp]
  insertAdd (s:^e) x = insertAdd' (s:^e) (extract s x)

  insertAdd' :: Exp -> (Maybe Int, [Exp]) -> [Exp]
  insertAdd' v (Nothing, x)      = v:x
  insertAdd' (s:^e) (Just e', x) = (s :^ addInt e e') : x

  -- Merging

  mergeAdd :: [Exp] -> [Exp] -> [Exp]
  mergeAdd []   y = y
  mergeAdd (v:x) y = insertAdd v (mergeAdd x y)

  multUnit :: Unit -> Unit -> Unit
  multUnit (EL a) (EL b) = EL (cleanup (mergeAdd a b))

  -- Negation

  negExpList :: [Exp] -> [Exp]
  negExpList  []        = []
  negExpList ((s:^e):x) = (s :^ negInt e) : negExpList x

  recip :: Unit -> Unit
  recip (EL a) = EL (negExpList a)

  -- Cleanup of 0s

  cleanup :: [Exp] -> [Exp]
  cleanup []         = []
  cleanup ((s:^e):x) = if e == i0 then x else (s:^e) : cleanup x
  |]

type instance (a :: Unit) :==: (b :: Unit) = EqUnit a b

-- Pretty operators for combining types

type family (a :: Unit) * (b :: Unit) :: Unit
type instance a*b = MultUnit a b

type family (a :: Unit) / (b :: Unit) :: Unit
type instance a/b = MultUnit a (Recip b)

-- Type for tagging values with units

data a :@ (u :: Unit) = U a deriving Show

-- Units themselves

type Meter    = [u|m |]
type Kilogram = [u|kg|]
type Second   = [u|s |]
type Ampere   = [u|A |]
type Kelvin   = [u|K |]
type Candela  = [u|cd|]
type Newton   = [u|kg*m/s²|]

-- Type-safe unit calculations

add :: (Num a, (u :==: v) ~ True) => a:@u -> a:@v -> a:@u
add (U a) (U b) = U (a+b)

sub :: (Num a, (u :==: v) ~ True) => a:@u -> a:@v -> a:@u
sub (U a) (U b) = U (a-b)

mul :: Num a => a:@u -> a:@v -> a:@(u*v)
mul (U a) (U b) = U (a*b)

div :: Fractional a => a:@u -> a:@v -> a:@(u/v)
div (U a) (U b) = U (a/b)

-- Tests

test1 :: Double :@ Meter
test1 = (U 1 :: Double :@ Meter) `add` (U 2 :: Double :@ Meter)

test2 :: Double :@ Second
test2 = U 5

test3 :: ([u|foo*bar|] :==: [u|bar*foo|]) ~ True => ()
test3 = ()

test4 :: Double :@ (Meter*Second)
test4 = test1 `mul` test2

test5 :: ((Meter*Meter :==: [u|m²|]) ~ True) => ()
test5 = ()

test6 :: Double :@ (Meter/Second)
test6 = test4 `div` (test2 `mul` test2)

test7 :: Double :@ Kilogram
test7 = U 13.24

test8 :: Double :@ Newton
test8 = (test7 `mul` test6) `div` test2
