{-# LANGUAGE DataKinds, TemplateHaskell, TypeFamilies, TypeOperators
  , UndecidableInstances #-}
module Units.Types where

import Prelude hiding (Int)
import Data.Singletons

promote [d|
  -- Peano ℕ⁺

  data Nat = N0 | NS Nat deriving Eq

  addNat :: Nat -> Nat -> Nat
  addNat  N0    m = m
  addNat (NS n) m = NS (addNat n m)

  mulNat :: Nat -> Nat -> Nat
  mulNat  N0    _ = N0
  mulNat (NS n) m = addNat m (mulNat n m)

  n0, n1, n2, n3, n4 :: Nat
  n0 = N0; n1 = NS n0; n2 = NS n1; n3 = NS n2; n4 = NS n3

  -- Integers as ‘normal’ or ‘negative’, where negative is offset by -1

  data Int = Norm Nat | Neg Nat deriving Eq

  addInt :: Int -> Int -> Int
  addInt (Norm a ) (Norm b) = Norm (addNat a b)
  addInt (Neg  a ) (Neg  b) = Neg (NS (addNat a b))

  -- 0 + (1-b) = 1-b
  addInt (Norm N0) (Neg b) = Neg b

  -- (1+a) + (-1-0) = 1+a - 1 = a
  addInt (Norm (NS a)) (Neg N0) = Norm a

  -- (1+a) + (-1-(1+b)) = 1+a + (-2-b) = a + -1-b
  addInt (Norm (NS a)) (Neg (NS b)) = addInt (Norm a) (Neg b)

  subInt :: Int -> Int -> Int
  subInt a b = addInt a (negInt b)

  mulIntNat :: Int -> Nat -> Int
  mulIntNat (Norm a) b = Norm (mulNat a b)
  mulIntNat (Neg  a) b = subInt (Norm (mulNat a b)) (Norm b)

  negInt :: Int -> Int
  negInt (Norm  N0   ) = Norm N0
  negInt (Norm (NS a)) = Neg a
  negInt (Neg   a    ) = Norm (NS a)

  i0, i1, i2, i3, i4 :: Int
  i0 = Norm n0; i1 = Norm n1; i2 = Norm n2; i3 = Norm n3; i4 = Norm n4

  -- Type-level ‘characters’

  data TChar = CA | CB | CC | CD | CE | CF | CG | CH | CI | CJ | CK | CL | CM
             | CN | CO | CP | CQ | CR | CS | CT | CU | CV | CW | CX | CY | CZ
             | Ca | Cb | Cc | Cd | Ce | Cf | Cg | Ch | Ci | Cj | Ck | Cl | Cm
             | Cn | Co | Cp | Cq | Cr | Cs | Ct | Cu | Cv | Cw | Cx | Cy | Cz

    deriving (Show, Eq, Ord)
  |]
