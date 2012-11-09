{-# LANGUAGE DataKinds, TemplateHaskell, TypeFamilies, TypeOperators
  , UndecidableInstances #-}
module Units.Types where

import Prelude hiding (Int)
import Data.Singletons

import qualified GHC.TypeLits as GHC (Nat)

promote [d|
  -- Peano ℕ⁺

  data Nat = N0 | NS Nat deriving Eq

  addNat :: Nat -> Nat -> Nat
  addNat  N0    m = m
  addNat (NS n) m = NS (addNat n m)

  mulNat :: Nat -> Nat -> Nat
  mulNat  N0    _ = N0
  mulNat (NS n) m = addNat m (mulNat n m)

  n1, n2, n3, n4, n5, n6, n7, n8, n9 :: Nat
  n1 = NS N0; n2 = NS n1; n3 = NS n2; n4 = NS n3; n5 = NS n4; n6 = NS n5;
  n7 = NS n6; n8 = NS n7; n9 = NS n8

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

  -- (-1-a) + b = b + (-1-a)
  addInt (Neg a) (Norm b) = addInt (Norm b) (Neg a)

  subInt :: Int -> Int -> Int
  subInt a b = addInt a (negInt b)

  mulInt :: Int -> Int -> Int
  mulInt (Norm a) (Norm b) = Norm (mulNat a b)
  mulInt (Neg  a) (Norm b) = subInt (Norm (mulNat a b)) (Norm b)
  mulInt (Norm a) (Neg  b) = mulInt (Neg b) (Norm a)

  -- (-1-a) * (-1-b) = -(-1-a) - b(-1-a) = 1+a+b+ab
  mulInt (Neg  a) (Neg  b) = Norm (NS (addNat a (addNat b (mulNat a b))))

  negInt :: Int -> Int
  negInt (Norm  N0   ) = Norm N0
  negInt (Norm (NS a)) = Neg a
  negInt (Neg   a    ) = Norm (NS a)

  i0, i1, i2, i3, i4, i5, i6, i7, i8, i9 :: Int
  i0 = Norm N0; i1 = Norm n1; i2 = Norm n2; i3 = Norm n3; i4 = Norm n4
  i5 = Norm n5; i6 = Norm n6; i7 = Norm n7; i8 = Norm n8; i9 = Norm n9

  im1, im2, im3, im4, im5 :: Int
  im1 = Neg N0; im2 = Neg n1; im3 = Neg n2; im4 = Neg n3; im5 = Neg n4

  -- Pretty association lists for units

  data Assoc = [TChar] :^ Int deriving Eq
  data Unit = EL [Assoc]

  key :: Assoc -> [TChar]
  key (s:^_) = s

  val :: Assoc -> Int
  val (_:^e) = e

  -- Type-level ‘characters’

  data TChar = CA | CB | CC | CD | CE | CF | CG | CH | CI | CJ | CK | CL | CM
             | CN | CO | CP | CQ | CR | CS | CT | CU | CV | CW | CX | CY | CZ
             | Ca | Cb | Cc | Cd | Ce | Cf | Cg | Ch | Ci | Cj | Ck | Cl | Cm
             | Cn | Co | Cp | Cq | Cr | Cs | Ct | Cu | Cv | Cw | Cx | Cy | Cz
    deriving (Show, Eq, Ord)
  |]

-- Type for tagging values with units

data a :@ (u :: Unit) = U a deriving Show
infix 5 :@

type One = EL '[]

-- Injection of built-int Nat -> Int

type family IntLit (a :: GHC.Nat) :: Int
type instance IntLit 0 = I0
type instance IntLit 1 = I1
type instance IntLit 2 = I2
type instance IntLit 3 = I3
type instance IntLit 4 = I4
type instance IntLit 5 = I5
type instance IntLit 6 = I6
type instance IntLit 7 = I7
type instance IntLit 8 = I8
type instance IntLit 9 = I9
