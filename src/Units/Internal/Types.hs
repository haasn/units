{-# LANGUAGE DataKinds, TemplateHaskell, TypeFamilies, TypeOperators
  , UndecidableInstances, PolyKinds, GADTs, RankNTypes, ScopedTypeVariables
  , FlexibleInstances, LambdaCase, ConstraintKinds #-}
module Units.Internal.Types
  ( module Units.Internal.Types
  , Symbol
  ) where

import Prelude hiding (Int)

import Data.Singletons
import Data.Singletons.Prelude()
import GHC.TypeLits hiding ((+)(),(-)(),(*)())
import qualified GHC.TypeLits as GHC

import Data.List (sortBy, partition, intersperse)
import Data.Function (on)

-- Pretty operators for combining types

type family (n :: k) + (m :: k) :: k; infixl 6 +
type family (n :: k) * (m :: k) :: k; infixl 7 *
type family (n :: k) - (m :: k) :: k; infixl 6 -
type family (n :: k) / (m :: k) :: k; infixl 7 /

type instance n + m = n GHC.+ m
type instance n * m = n GHC.* m
type instance n - m = n GHC.- m

-- Integers as ‘normal’ or ‘negative’, where negative is offset by -1

data Int = Norm Nat | Neg Nat

data instance Sing (i :: Int) where
  SNorm :: Sing n -> Sing (Norm n)
  SNeg  :: Sing n -> Sing (Neg  n)

instance SingI n => SingI (Norm n) where sing = SNorm sing
instance SingI n => SingI (Neg  n) where sing = SNeg sing

instance SingKind ('KProxy :: KProxy Int) where
  type DemoteRep ('KProxy :: KProxy Int) = Integer
  fromSing (SNorm n) = fromSing n
  fromSing (SNeg  n) = (-1) - fromSing n

type family AddInt a b where
  Norm a `AddInt` Norm b = Norm (a + b)
  Neg  a `AddInt` Neg  b = Neg  (a + b + 1)

  -- 0 + (1-b) = 1-b
  Norm 0 `AddInt` Neg b = Neg b

  -- (1+a) + (-1-0) = 1+a - 1 = a
  Norm a `AddInt` Neg 0 = Norm (a - 1)

  -- (1+a) + (-1-(1+b)) = 1+a + (-2-b) = a + -1-b
  Norm a `AddInt` Neg b = Norm (a - 1) + Neg (b - 1)

  -- (-1-a) + b = b + (-1-a)
  Neg a `AddInt` Norm b = Norm b + Neg a

type instance a + b = a `AddInt` b

type family Negate (a :: Int) :: Int where
  Negate (Norm  0) = Norm 0
  Negate (Norm  a) = Neg (a - 1)
  Negate (Neg   a) = Norm (a + 1)

type instance a - b = a + Negate b

type family MulInt a b where
  Norm a `MulInt` Norm b = Norm (a * b)
  Neg  a `MulInt` Norm b = Negate (Norm (a * b + b))
  Norm a `MulInt` Neg  b = Neg b * Norm a

  -- (-1-a) * (-1-b) = -(-1-a) - b(-1-a) = 1+a+b+ab
  Neg a `MulInt` Neg b = Norm (a + b + a * b + 1)

type instance a * b = a `MulInt` b

-- Pretty association lists for units

data Assoc = Symbol :^ Int
data Unit = EL [Assoc]

data instance Sing (a :: Assoc) where SAssoc :: Sing t -> Sing i -> Sing (t :^ i)
data instance Sing (u :: Unit ) where SEL :: Sing as -> Sing (EL as)

instance (SingI t, SingI i) => SingI (t :^ i) where sing = SAssoc sing sing
instance SingI as => SingI (EL as) where sing = SEL sing

instance SingKind ('KProxy :: KProxy Assoc) where
  type DemoteRep ('KProxy :: KProxy Assoc) = (String, Integer)
  fromSing (SAssoc t i) = (fromSing t, fromSing i)

instance SingKind ('KProxy :: KProxy Unit) where
  type DemoteRep ('KProxy :: KProxy Unit) = [(String, Integer)]
  fromSing (SEL as) = fromSing as

type SingRep e = (SingI e, SingKind (KindOf e))

-- | Type for tagging values with units. Use 'lit' for constructing values
--   of this type.

data a :@ (u :: Unit) = U a
infix 5 :@

-- Pretty printing of units

instance (Show a, SingRep u) => Show (a :@ u) where
  show u@(U x) = show x ++ " " ++ showUnit (fromSing (sing :: Sing u))

showUnit :: [(String,Integer)] -> String
showUnit s = showEL pos `or` "1" ++ dash ++ brL ++ showEL (map negA neg) ++ brR
 where
  (pos, neg) = partition (\(u,e) -> e>0 || u == "deca") s
  (brL, brR) = if length neg > 1 then ("(",")") else ("","")
  dash       = if length neg > 0 then "/" else ""
  negA (s,n) = (s, -n)

  [] `or` ys = ys
  xs `or` _  = xs

  -- Treat multiplication signs as phantom units so they can be removed
  -- where appropriate before processing SI prefixes
  showEL = concat . map showAssoc . stripSI . intersperse ("·",0)
           . sortBy (flip compare `on` abs . snd)

  stripSI (("deca",n):("·",0):xs) = ("deca",n) : stripSI xs
  stripSI (a:xs) = a : stripSI xs
  stripSI []     = []

  showExp = \case
    1 -> "" ; 2 -> "²"; 3 -> "³"; 4 -> "⁴"; 5 -> "⁵"; 6 -> "⁶"; 7 -> "⁷"
    8 -> "⁸"; 0 -> "" ; e -> '^' : show e

  -- Special casing of ‘deca’ to show SI prefixes with their proper names
  showAssoc ("deca", n) = case n of
    1   -> "da";  2  -> "hekto";  3  -> "k";  6  -> "M";  9  -> "G";  12 -> "T"
    15  -> "P" ;  18 -> "E";  21 -> "Z";  24 -> "Y"; -1  -> "d"; -2  -> "c"
    -3  -> "m" ; -6  -> "μ"; -9  -> "n"; -12 -> "p"; -15 -> "f"; -18 -> "a"
    -21 -> "z" ; -24 -> "y";  e  -> "10" ++ showExp e ++ "·"

  showAssoc (s, n) = s ++ showExp n
