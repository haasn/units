{-# LANGUAGE DataKinds, TemplateHaskell, TypeFamilies, TypeOperators
  , UndecidableInstances, PolyKinds, GADTs, RankNTypes #-}
module Units.Internal.Types where

import Prelude hiding (Int)
import Units.Internal.TypeOrd

-- import GHC.TypeLits hiding ((+)(),(-)(),(*)(),Nat)
import qualified GHC.TypeLits as GHC (Nat)

-- Pretty operators for combining types

type family (n :: k) + (m :: k) :: k; infixl 6 +
type family (n :: k) * (m :: k) :: k; infixl 7 *
type family (n :: k) - (m :: k) :: k; infixl 6 -
type family (n :: k) / (m :: k) :: k; infixl 7 /

-- Peano ℕ

data Nat = N0 | NS Nat deriving Eq

type instance where
  N0   + m = m
  NS n + m = NS (n+m)

type instance where
  N0   * m = N0
  NS n * m = m + (n*m)

type instance where
  Compare  N0     N0    = EQ
  Compare  N0     m     = LT
  Compare  n      N0    = GT
  Compare (NS n) (NS m) = Compare n m

type N1 = NS N0
type N2 = NS N1
type N3 = NS N2
type N4 = NS N3
type N5 = NS N4
type N6 = NS N5
type N7 = NS N6
type N8 = NS N7
type N9 = NS N8

-- Integers as ‘normal’ or ‘negative’, where negative is offset by -1

data Int = Norm Nat | Neg Nat deriving Eq

type instance where
  Norm a + Norm b = Norm (a+b)
  Neg  a + Neg  b = Neg  (NS (a+b))

  -- 0 + (1-b) = 1-b
  Norm N0 + Neg b = Neg b

  -- (1+a) + (-1-0) = 1+a - 1 = a
  Norm (NS a) + Neg N0 = Norm a

  -- (1+a) + (-1-(1+b)) = 1+a + (-2-b) = a + -1-b
  Norm (NS a) + Neg (NS b) = Norm a + Neg b

  -- (-1-a) + b = b + (-1-a)
  Neg a + Norm b = Norm b + Neg a

type family Negate (a :: Int) :: Int
type instance where
  Negate (Norm  N0   ) = Norm N0
  Negate (Norm (NS a)) = Neg a
  Negate (Neg   a    ) = Norm (NS a)

type instance where
  a - b = a + Negate b

type instance where
  Norm a * Norm b = Norm (a*b)
  Neg  a * Norm b = Negate (Norm (NS a * b))
  Norm a * Neg  b = Neg b * Norm a

  -- (-1-a) * (-1-b) = -(-1-a) - b(-1-a) = 1+a+b+ab
  Neg a * Neg b = Norm (NS (a + b + a*b))

type IM1 = Neg N0
type I0  = Norm N0
type I1  = Norm N1
type I2  = Norm N2
type I3  = Norm N3
type I4  = Norm N4
type I5  = Norm N5
type I6  = Norm N6
type I7  = Norm N7
type I8  = Norm N8
type I9  = Norm N9

-- Pretty association lists for units

data Assoc = [TChar] :^ Int deriving Eq
data Unit = EL [Assoc]

-- Type-level ‘characters’

data TChar = CA | CB | CC | CD | CE | CF | CG | CH | CI | CJ | CK | CL | CM
           | CN | CO | CP | CQ | CR | CS | CT | CU | CV | CW | CX | CY | CZ
           | Ca | Cb | Cc | Cd | Ce | Cf | Cg | Ch | Ci | Cj | Ck | Cl | Cm
           | Cn | Co | Cp | Cq | Cr | Cs | Ct | Cu | Cv | Cw | Cx | Cy | Cz
  deriving (Show, Eq, Ord)

-- | Type for tagging values with units. Use 'lit' for constructing values
--   of this type.

data a :@ (u :: Unit) = U a
infix 5 :@

instance Show a => Show (a :@ u) where
  show (U x) = show x

-- Injection of built-int Nat -> Int, ugly at the moment due to lack of
-- proper Nat operators

type family IntLit (a :: GHC.Nat) :: Int
type instance where
  IntLit 0 = I0
  IntLit 1 = I1
  IntLit 2 = I2
  IntLit 3 = I3
  IntLit 4 = I4
  IntLit 5 = I5
  IntLit 6 = I6
  IntLit 7 = I7
  IntLit 8 = I8
  IntLit 9 = I9

-- Comparison of type-level characters and associations

makeOrd ''TChar

type instance Compare (a :^ e) (b :^ f) = Compare a b
