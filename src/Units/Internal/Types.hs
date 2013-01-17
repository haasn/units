{-# LANGUAGE DataKinds, TemplateHaskell, TypeFamilies, TypeOperators
  , UndecidableInstances, PolyKinds, GADTs, RankNTypes, ScopedTypeVariables
  , FlexibleInstances, LambdaCase #-}
module Units.Internal.Types where

import Prelude hiding (Int)
import Units.Internal.TypeOrd

import GHC.TypeLits hiding ((+)(),(-)(),(*)(),Nat)
import qualified GHC.TypeLits as GHC (Nat)

-- Pretty operators for combining types

type family (n :: k) + (m :: k) :: k; infixl 6 +
type family (n :: k) * (m :: k) :: k; infixl 7 *
type family (n :: k) - (m :: k) :: k; infixl 6 -
type family (n :: k) / (m :: k) :: k; infixl 7 /

-- Peano ℕ

data Nat = N0 | NS Nat deriving Eq

data instance Sing (n :: Nat) where
  SN0 :: Sing N0
  SNS :: Sing n -> Sing (NS n)

instance SingI N0 where sing = SN0
instance SingI n => SingI (NS n) where sing = SNS sing

instance SingE (KindOf N0) where
  type DemoteRep (KindOf N0) = Integer
  fromSing  SN0    = 0
  fromSing (SNS n) = 1 + fromSing n

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

data instance Sing (i :: Int) where
  SNorm :: Sing n -> Sing (Norm n)
  SNeg  :: Sing n -> Sing (Neg  n)

instance SingI n => SingI (Norm n) where sing = SNorm sing
instance SingI n => SingI (Neg  n) where sing = SNeg sing

instance SingE (KindOf I0) where
  type DemoteRep (KindOf I0) = Integer
  fromSing (SNorm n) = fromSing n
  fromSing (SNeg  n) = (-1) - fromSing n

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

data instance Sing (a :: Assoc) where SAssoc :: Sing t -> Sing i -> Sing (t :^ i)
data instance Sing (u :: Unit ) where SEL :: Sing as -> Sing (EL as)

instance (SingI t, SingI i) => SingI (t :^ i) where sing = SAssoc sing sing
instance SingI as => SingI (EL as) where sing = SEL sing

instance SingE (KindParam :: OfKind Assoc) where
  type DemoteRep (KindParam :: OfKind Assoc) = (String, Integer)
  fromSing (SAssoc t i) = (fromSing t,  fromSing i)

instance SingE (KindParam :: OfKind Unit) where
  type DemoteRep (KindParam :: OfKind Unit) = [(String, Integer)]
  fromSing (SEL as) = fromSing as

-- Kind-polymorphic proxy

data Proxy k = Proxy

-- Type-level ‘characters’

data TChar = CA | CB | CC | CD | CE | CF | CG | CH | CI | CJ | CK | CL | CM
           | CN | CO | CP | CQ | CR | CS | CT | CU | CV | CW | CX | CY | CZ
           | Ca | Cb | Cc | Cd | Ce | Cf | Cg | Ch | Ci | Cj | Ck | Cl | Cm
           | Cn | Co | Cp | Cq | Cr | Cs | Ct | Cu | Cv | Cw | Cx | Cy | Cz
  deriving (Show, Eq, Ord)

data instance Sing (c :: TChar) where
  SChar :: Sing (s :: Symbol) -> Sing (c :: TChar)

instance SingI (R c) => SingI (c :: TChar) where
  sing = SChar (sing :: Sing (R c))

instance SingE (KindOf CA) where
  type DemoteRep (KindOf CA) = Char
  fromSing (SChar s) = head (fromSing s)

data instance Sing (l :: [a]) where
  SNil  :: Sing '[]
  SCons :: Sing a -> Sing as -> Sing (a ': as)

instance SingI '[] where
  sing = SNil

instance (SingI a, SingI as) => SingI (a ': as) where
  sing = SCons sing sing

instance SingE (KindParam :: OfKind a) => SingE (KindParam :: OfKind [a]) where
  type DemoteRep (KindParam :: OfKind [a]) = [DemoteRep (KindParam :: OfKind a)]
  fromSing SNil = []
  fromSing (SCons x xs) = fromSing x : fromSing xs

type family R (c :: TChar) :: Symbol
type instance where
  R CA = "A"; R CB = "B"; R CC = "C"; R CD = "D"; R CE = "E"; R CF = "F"
  R CG = "G"; R CH = "H"; R CI = "I"; R CJ = "J"; R CK = "K"; R CL = "L"
  R CM = "M"; R CN = "N"; R CO = "O"; R CP = "P"; R CQ = "Q"; R CR = "R"
  R CS = "S"; R CT = "T"; R CU = "U"; R CV = "V"; R CW = "W"; R CX = "X"
  R CY = "Y"; R CZ = "Z"; R Ca = "a"; R Cb = "b"; R Cc = "c"; R Cd = "d"
  R Ce = "e"; R Cf = "f"; R Cg = "g"; R Ch = "h"; R Ci = "i"; R Cj = "j"
  R Ck = "k"; R Cl = "l"; R Cm = "m"; R Cn = "n"; R Co = "o"; R Cp = "p"
  R Cq = "q"; R Cr = "r"; R Cs = "s"; R Ct = "t"; R Cu = "u"; R Cv = "v"
  R Cw = "w"; R Cx = "x"; R Cy = "y"; R Cz = "z"

-- | Type for tagging values with units. Use 'lit' for constructing values
--   of this type.

data a :@ (u :: Unit) = U a
infix 5 :@

-- Pretty printing of units

instance (Show a, SingRep u) => Show (a :@ u) where
  show u@(U x) = show x ++ " " ++ showUnit (fromSing (sing :: Sing u))

showUnit :: [(String,Integer)] -> String
showUnit = foldr (\(s,e) r -> s ++ "·" ++ showExp e ++ r) ""
 where
  showExp = \case
    1 -> "" ; 2 -> "²"; 3 -> "³"; 4 -> "⁴"; 5 -> "⁵"; 6 -> "⁶"; 7 -> "⁷"
    8 -> "⁸"; e -> '^' : show e

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
