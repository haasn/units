{-# LANGUAGE DataKinds, TemplateHaskell, TypeFamilies, TypeOperators
  , UndecidableInstances, PolyKinds #-}
module Units.Internal.Types where

import Prelude hiding (Int)
import Data.Singletons
import Units.Internal.TypeOrd

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

  cmpNat :: Nat -> Nat -> Ordering
  cmpNat  N0     N0    = EQ
  cmpNat  N0    (NS _) = LT
  cmpNat (NS _)  N0    = GT
  cmpNat (NS a) (NS b) = cmpNat a b

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

  mulInt (Neg  a) (Norm b) = subInt i0 (Norm (mulNat (NS a) b))
  mulInt (Norm a) (Neg  b) = mulInt (Neg b) (Norm a)

  -- (-1-a) * (-1-b) = -(-1-a) - b(-1-a) = 1+a+b+ab
  mulInt (Neg  a) (Neg  b) = Norm (NS (addNat a (addNat b (mulNat a b))))

  negInt :: Int -> Int
  negInt (Norm  N0   ) = Norm N0
  negInt (Norm (NS a)) = Neg a
  negInt (Neg   a    ) = Norm (NS a)

  absInt :: Int -> Int
  absInt (Norm x) = Norm x
  absInt (Neg  x) = Norm (NS x)

  signumInt :: Int -> Int
  signumInt (Norm  N0   ) = i0
  signumInt (Norm (NS _)) = i1
  signumInt (Neg   _    ) = im1

  cmpInt :: Int -> Int -> Ordering
  cmpInt (Norm a) (Norm b) = cmpNat a b
  cmpInt (Neg  a) (Neg  b) = cmpNat b a
  cmpInt (Norm _) (Neg  _) = GT
  cmpInt (Neg  _) (Norm _) = LT

  lessThan :: Int -> Int -> Bool
  lessThan a b = lT' (cmpInt a b)

  lT' :: Ordering -> Bool
  lT' LT = True
  lT' EQ = False
  lT' GT = False

  i0, i1, i2, i3, i4, i5, i6, i7, i8, i9 :: Int
  i0 = Norm N0; i1 = Norm n1; i2 = Norm n2; i3 = Norm n3; i4 = Norm n4
  i5 = Norm n5; i6 = Norm n6; i7 = Norm n7; i8 = Norm n8; i9 = Norm n9

  im1, im2, im3, im4, im5 :: Int
  im1 = Neg N0; im2 = Neg n1; im3 = Neg n2; im4 = Neg n3; im5 = Neg n4

  -- Rational numbers, as pairs of ints a/b

  data Rational = Int :/ Int deriving Eq

  mkRatio :: Int -> Int -> Rational
  mkRatio x y = reduce (mulInt x (signumInt y)) (absInt y)

  reduce :: Int -> Int -> Rational
  reduce x y = reduce' x y (gcdInt x y)

  reduce' :: Int -> Int -> Int -> Rational
  reduce' x y g = (x `quotInt` g) :/ (y `quotInt` g)

  gcdInt :: Int -> Int -> Int
  gcdInt x y = gcdInt' (absInt x) (absInt y)

  gcdInt' :: Int -> Int -> Int
  gcdInt' a (Norm  N0   ) = a
  gcdInt' a (Norm (NS b)) = gcdInt' (Norm (NS b)) (a `remInt` Norm (NS b))
  gcdInt' a (Neg   b    ) = gcdInt' (Neg b) (a `remInt` Neg b)

  quotInt :: Int -> Int -> Int
  quotInt x y = fst' (quotRemInt x y)

  remInt :: Int -> Int -> Int
  remInt x y = snd' (quotRemInt x y)

  fst' :: (a, b) -> a
  fst' (a, _) = a

  snd' :: (a, b) -> b
  snd' (_, b) = b

  quotRemInt :: Int -> Int -> (Int, Int)
  quotRemInt a b = mulBoth (mulInt (signumInt a) (signumInt b)) (signumInt a)
                           (qRI' (absInt a) (absInt b) (absInt a) (absInt b))

  qRI' :: Int -> Int -> Int -> Int -> (Int, Int)
  -- a < b
  qRI' (Neg  _    ) (Norm (NS _)) a _ = (i0, a)
  qRI' (Norm N0   ) (Norm (NS _)) a _ = (i0, a)
  qRI' (Neg (NS _)) (Neg   N0   ) a _ = (i0, a)

  -- a >= b
  qRI' (Norm _    ) (Neg   _    ) a b = qRI'' b (qRI''' a (mulInt i2 b))
  qRI' (Norm _    ) (Norm  N0   ) a b = qRI'' b (qRI''' a (mulInt i2 b))
  qRI' (Neg  N0   ) (Neg   _    ) a b = qRI'' b (qRI''' a (mulInt i2 b))

  -- a ? b
  qRI' (Norm (NS a')) (Norm (NS b')) a b = qRI' (Norm a') (Norm b') a b
  qRI' (Neg  (NS a')) (Neg  (NS b')) a b = qRI' (Neg  a') (Neg  b') a b

  qRI'' :: Int -> (Int, Int) -> (Int, Int)
  qRI'' b (q', r') = if not (r' `lessThan` b)
                       then ( addInt i1 (mulInt i2 q')
                            , addInt r' (negInt b))
                       else (mulInt i2 q', r')

  qRI''' :: Int -> Int -> (Int, Int)
  qRI''' a b = qRI' a b a b

  mulBoth :: Int -> Int -> (Int, Int) -> (Int, Int)
  mulBoth a b (x, y) = (mulInt a x, mulInt b y)

  addRat :: Rational -> Rational -> Rational
  addRat (x :/ y) (x' :/ y') = reduce (addInt (mulInt x y') (mulInt x' y))
                                      (mulInt y y')

  mulRat :: Rational -> Rational -> Rational
  mulRat (x :/ y) (x' :/ y') = reduce (mulInt x x') (mulInt y y')

  negRat :: Rational -> Rational
  negRat (x :/ y) = negInt x :/ y

  r0, r1, r2, r3, r4, r5, r6, r7, r8, r9 :: Rational
  r0 = i0:/i1; r1 = i1:/i1; r2 = i2:/i1; r3 = i3:/i1; r4 = i4:/i1; r5 = i5:/i1
  r6 = i6:/i1; r7 = i7:/i1; r8 = i8:/i1; r9 = i9:/i1

  rm1, rm2, rm3, rm4, rm5 :: Rational
  rm1 = im1:/i1; rm2 = im2:/i1; rm3 = im3:/i1; rm4 = im4:/i1; rm5 = im5:/i1

  -- Pretty association lists for units

  data Assoc = [TChar] :^ Rational deriving Eq
  data Unit = EL [Assoc]

  key :: Assoc -> [TChar]
  key (s:^_) = s

  val :: Assoc -> Rational
  val (_:^e) = e

  -- Type-level ‘characters’

  data TChar = CA | CB | CC | CD | CE | CF | CG | CH | CI | CJ | CK | CL | CM
             | CN | CO | CP | CQ | CR | CS | CT | CU | CV | CW | CX | CY | CZ
             | Ca | Cb | Cc | Cd | Ce | Cf | Cg | Ch | Ci | Cj | Ck | Cl | Cm
             | Cn | Co | Cp | Cq | Cr | Cs | Ct | Cu | Cv | Cw | Cx | Cy | Cz
    deriving (Show, Eq, Ord)
  |]

-- | Type for tagging values with units. Use 'lit' for constructing values
--   of this type.

data a :@ (u :: Unit) = U a
infix 5 :@

instance Show a => Show (a :@ u) where
  show (U x) = show x

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

-- Comparison of type-level characters and associations

makeOrd ''TChar

type instance Compare (a :^ e) (b :^ f) = Compare a b

-- Sorting algorithm for type-level lists

sort :: [Assoc] -> [Assoc]
sort = undefined -- Just get this here for the promotion

type family Sort (xs :: [k]) :: [k]

type instance Sort    '[]    = '[]
type instance Sort (x ': xs) = Insert x (Sort xs)

type family Insert (x :: k) (xs :: [k]) :: [k]

type instance Insert x    '[]    = '[x]
type instance Insert x (y ': ys) = Insert' (Compare x y) x y ys

type family Insert' (o :: Ordering) (x :: k) (y :: k) (ys :: [k]) :: [k]

type instance Insert' EQ x y ys = x ': y ': ys
type instance Insert' LT x y ys = x ': y ': ys
type instance Insert' GT x y ys = y ': Insert x ys
