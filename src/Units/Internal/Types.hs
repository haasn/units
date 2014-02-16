{-# LANGUAGE DataKinds, TemplateHaskell, TypeFamilies, TypeOperators
  , UndecidableInstances, PolyKinds, GADTs, RankNTypes, ScopedTypeVariables
  , FlexibleInstances, LambdaCase, ConstraintKinds #-}
module Units.Internal.Types where

import Prelude hiding (Int)
import Units.Internal.TypeOrd

import Data.Singletons
import GHC.TypeLits hiding ((+)(),(-)(),(*)(),Nat)
import qualified GHC.TypeLits as GHC (Nat)

import Data.List (sortBy, partition, intersperse)
import Data.Function (on)

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

instance SingKind (KindOf N0) where
  type DemoteRep (KindOf N0) = Integer
  fromSing  SN0    = 0
  fromSing (SNS n) = 1 + fromSing n

type family AddNat a b where
  N0   `AddNat` m = m
  NS n `AddNat` m = NS (n `AddNat` m)

type instance n + m = n `AddNat` m

type family MulNat a b where
  N0   `MulNat` m = N0
  NS n `MulNat` m = m `AddNat` (n `MulNat` m)

type instance n * m = n `MulNat` m

type family CmpNat a b where
  CmpNat  N0     N0    = EQ
  CmpNat  N0     m     = LT
  CmpNat  n      N0    = GT
  CmpNat (NS n) (NS m) = CmpNat n m

type instance Compare n m = CmpNat n m

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

instance SingKind (KindOf I0) where
  type DemoteRep (KindOf I0) = Integer
  fromSing (SNorm n) = fromSing n
  fromSing (SNeg  n) = (-1) - fromSing n

type family AddInt a b where
  Norm a `AddInt` Norm b = Norm (a+b)
  Neg  a `AddInt` Neg  b = Neg  (NS (a+b))

  -- 0 + (1-b) = 1-b
  Norm N0 `AddInt` Neg b = Neg b

  -- (1+a) + (-1-0) = 1+a - 1 = a
  Norm (NS a) `AddInt` Neg N0 = Norm a

  -- (1+a) + (-1-(1+b)) = 1+a + (-2-b) = a + -1-b
  Norm (NS a) `AddInt` Neg (NS b) = Norm a + Neg b

  -- (-1-a) + b = b + (-1-a)
  Neg a `AddInt` Norm b = Norm b + Neg a

type instance a + b = a `AddInt` b

type family Negate (a :: Int) :: Int where
  Negate (Norm  N0   ) = Norm N0
  Negate (Norm (NS a)) = Neg a
  Negate (Neg   a    ) = Norm (NS a)

type instance a - b = a + Negate b

type family MulInt a b where
  Norm a `MulInt` Norm b = Norm (a*b)
  Neg  a `MulInt` Norm b = Negate (Norm (NS a * b))
  Norm a `MulInt` Neg  b = Neg b * Norm a

  -- (-1-a) * (-1-b) = -(-1-a) - b(-1-a) = 1+a+b+ab
  Neg a `MulInt` Neg b = Norm (NS (a + b + a*b))

type instance a * b = a `MulInt` b

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

instance SingKind ('KProxy :: KProxy Assoc) where
  type DemoteRep ('KProxy :: KProxy Assoc) = (String, Integer)
  fromSing (SAssoc t i) = (fromSing t,  fromSing i)

instance SingKind ('KProxy :: KProxy Unit) where
  type DemoteRep ('KProxy :: KProxy Unit) = [(String, Integer)]
  fromSing (SEL as) = fromSing as

-- Type-level ‘characters’

data TChar = CA | CB | CC | CD | CE | CF | CG | CH | CI | CJ | CK | CL | CM
           | CN | CO | CP | CQ | CR | CS | CT | CU | CV | CW | CX | CY | CZ
           | Ca | Cb | Cc | Cd | Ce | Cf | Cg | Ch | Ci | Cj | Ck | Cl | Cm
           | Cn | Co | Cp | Cq | Cr | Cs | Ct | Cu | Cv | Cw | Cx | Cy | Cz
  deriving (Show, Eq, Ord)

-- Represent TChar's Sing as a wrapper around a Symbol internally, then
-- construct/demote that instead.

data instance Sing (c :: TChar) where
  SChar :: Sing (s :: Symbol) -> Sing (c :: TChar)

instance SingI (R c) => SingI (c :: TChar) where
  sing = SChar (sing :: Sing (R c))

instance SingKind (KindOf CA) where
  type DemoteRep (KindOf CA) = Char
  fromSing (SChar s) = head (fromSing s)

{-
data instance Sing (l :: [a]) where
  SNil  :: Sing '[]
  SCons :: Sing a -> Sing as -> Sing (a ': as)

instance SingI '[] where
  sing = SNil

instance (SingI a, SingI as) => SingI (a ': as) where
  sing = SCons sing sing

instance SingKind (KindOf a) => SingKind (KindOf [a]) where
  type DemoteRep (KindOf [a]) = [DemoteRep (KindOf a)]
  fromSing SNil = []
  fromSing (SCons x xs) = fromSing x : fromSing xs
-}

-- Reflect a TChar to its Symbol representation, for demotion
type family R (c :: TChar) :: Symbol where
  R CA = "A"; R CB = "B"; R CC = "C"; R CD = "D"; R CE = "E"; R CF = "F"
  R CG = "G"; R CH = "H"; R CI = "I"; R CJ = "J"; R CK = "K"; R CL = "L"
  R CM = "M"; R CN = "N"; R CO = "O"; R CP = "P"; R CQ = "Q"; R CR = "R"
  R CS = "S"; R CT = "T"; R CU = "U"; R CV = "V"; R CW = "W"; R CX = "X"
  R CY = "Y"; R CZ = "Z"; R Ca = "a"; R Cb = "b"; R Cc = "c"; R Cd = "d"
  R Ce = "e"; R Cf = "f"; R Cg = "g"; R Ch = "h"; R Ci = "i"; R Cj = "j"
  R Ck = "k"; R Cl = "l"; R Cm = "m"; R Cn = "n"; R Co = "o"; R Cp = "p"
  R Cq = "q"; R Cr = "r"; R Cs = "s"; R Ct = "t"; R Cu = "u"; R Cv = "v"
  R Cw = "w"; R Cx = "x"; R Cy = "y"; R Cz = "z"

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

-- Injection of built-int Nat -> Int, ugly at the moment due to lack of
-- proper Nat operators.

type family IntLit (a :: GHC.Nat) :: Int where
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
