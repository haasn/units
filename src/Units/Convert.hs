{-# LANGUAGE ConstraintKinds, TypeFamilies, TypeOperators, DataKinds
  , FlexibleInstances, TemplateHaskell, TypeSynonymInstances, PolyKinds
  , UndecidableInstances, ScopedTypeVariables, NoMonomorphismRestriction #-}
-- | Attempt at a class-based unit conversion system. Current limitations
--   are:
--
--   * Even fully applied type synonym families can't be made into instances,
--     so you have to expand the units to their underlying representation
--     manually.
--
--   * Type synonym families aren't reversible (Haskell isn't a constraint
--     solver), so compound units can't be converted - they have to be taken
--     apart and converted on an ad-hod basis.
--
--   The way the class works, to prevent exponential instance numbers like
--   MPTC-based approaches need, is by converting to and from a common ‘base’
--   unit that is used as common denominator between the two. In addition to
--   the class, this module exports a bunch of arbitrarily chosen base units,
--   defined as the SI units, as well as constraints for asserting something
--   to be isomorphic to them.

module Units.Convert where

import Prelude hiding (Int)

import Data.Singletons

import Units.Internal.Types
import GHC.Exts (Constraint)

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Units

-- We need a kind-polymorphic version
data Proxy k = Proxy

class IsoDim (u :: [TChar]) where
  type From u :: [TChar]

  factor :: Fractional a => p u -> a -- From u / u

type family Base (u :: Unit) :: Unit
type instance Base (EL u) = EL (Cleanup (Merge (Sort (Strip (Base' u)))))

-- Take each primitive unit to its base form
type family Base' (u :: [Assoc]) :: [Assoc]
type instance Base' '[]          = '[]
type instance Base' ((u:^e)':us) = (From u :^ e) ': Base' us

-- Get rid of units with a dimensionless (empty) base
type family Strip (u :: [Assoc]) :: [Assoc]
type instance Strip '[] = '[]
type instance Strip (('[]      :^ e) ': us) = Strip us
type instance Strip (((n ':ns) :^ e) ': us) = ((n ':ns) :^ e) ': Strip us

-- Merge two adjacent units with the same base
type family Merge (u :: [Assoc]) :: [Assoc]
type instance Merge  '[ ]                = '[ ]
type instance Merge  '[x]                = '[x]
type instance Merge ((u:^e)':(v:^d)':us) =
  If (u :== v)
    ((u :^ AddInt e d)    ': Merge us)
    ((u :^ e) ': (v :^ d) ': Merge us)

-- Internal class for multiplying factors
class Factor (u :: Unit) where
  getFactor :: Fractional a => p u -> a

instance Factor (EL '[]) where
  getFactor _ = 1

instance (Factor (EL xs), GetInt e, IsoDim u)
         => Factor (EL ((u :^ e) ': xs)) where
  getFactor _ = factor (Proxy :: Proxy u) ^^ getInt (Proxy :: Proxy e)
                * getFactor (Proxy :: Proxy (EL xs))

-- Convert a type-Int to value level representation
class GetInt (u :: Int) where
  getInt :: p u -> Integer

instance GetNat n => GetInt (Norm n) where
  getInt _ = getNat (Proxy :: Proxy n)

instance GetNat n => GetInt (Neg n)
  where getInt _ = (-1) - getNat (Proxy :: Proxy n)

-- Convert a type-Nat to value level representation
class GetNat (u :: Nat) where
  getNat :: p u -> Integer

instance GetNat N0 where
  getNat _ = 0

instance GetNat n => GetNat (NS n) where
  getNat _ = 1 + getNat (Proxy :: Proxy n)

-- Convert between applicable units

type Convert u v = (Factor u, Factor v, Base u ~ Base v)

convert :: forall a u v. (Fractional a, Convert u v) => a :@ u -> a :@ v
convert (U n _) = U (n * f1 / f2) Nothing
  where f1 = getFactor (Proxy :: Proxy u)
        f2 = getFactor (Proxy :: Proxy v)

-- | Class for units that are isomorphic to some common base, with the
--   conversion requiring a certain context 'Ctx'. Use 'Empty' for none.

{-
class IsoUnit (u :: Unit) where
  type Base u :: Unit
  type Ctx  u :: * -> Constraint

  toBase   :: Ctx u a => a :@ u -> a :@ Base u
  fromBase :: Ctx u a => a :@ Base u -> a :@ u

-- | ‘Convert u j a’ requires IsoUnit u/j instances, asserts their bases as
--   equal, and pulls in both value contexts.

type Convert u j a = (IsoUnit u, IsoUnit j, Base u ~ Base j, Ctx u a, Ctx j a)

convert :: Convert u j a => a :@ u -> a :@ j
convert = fromBase . toBase

-- | An empty context, with a valid instance for any type.

class Empty a
instance Empty a

type UnitLike b u = (IsoUnit u, Base u ~ b)

-- This exports a bunch of base units to be assumed as universal for the
-- conversions to work properly. These are arbitrarily defined as the SI
-- units, mainly for efficiency reasons when dealing with more fundamental
-- units like the Planck units. Since type family instances are illegal, we
-- have to manually expand these here. :(

$(let
  makeBase :: String -> String -> Q [Dec]
  makeBase s su = do
    let n = mkName $ "Base" ++ s
    let ln = mkName $ s ++ "Like"
    ty <- quoteType u su
    a  <- newName "a"
    return
      [ TySynD n [] ty
      , TySynD ln [PlainTV a] (AppT (AppT (ConT ''UnitLike) (ConT n)) (VarT a))
      , InstanceD [] (AppT (ConT ''IsoUnit) (ConT n))
        [ TySynInstD ''Base [ConT n] (ConT n)
        , TySynInstD ''Ctx  [ConT n] (ConT ''Empty)
        , FunD 'toBase   [Clause [] (NormalB (VarE 'id)) []]
        , FunD 'fromBase [Clause [] (NormalB (VarE 'id)) []]
        ]
      ]

  bases :: [(String, String)]
  bases =
    [ ("Number"       , ""           )
    , ("Length"       , "m"          )
    , ("Area"         , "m²"         )
    , ("Volume"       , "m³"         )
    , ("Weight"       , "kg"         )
    , ("Time"         , "s"          )
    , ("Current"      , "A"          )
    , ("Temperature"  , "K"          )
    , ("Luminosity"   , "cd"         )
    , ("Frequency"    , "s^-1"       )
    , ("Speed"        , "m/s"        )
    , ("Acceleration" , "m/s²"       )
    , ("Momentum"     , "kg*m/s"     )
    , ("Force"        , "kg*m/s²"    )
    , ("Pressure"     , "kg/m/s²"    )
    , ("Energy"       , "kg*m²/s²"   )
    , ("Power"        , "kg*m²/s³"   )
    , ("Potential"    , "kg*m²/s³/A" )
    , ("Capacitance"  , "s⁴*A²/m²/kg")
    , ("Resistance"   , "kg*m²/s³/A²")
    , ("Conductance"  , "s³*A²/m²/kg")
    , ("MagneticFlux" , "kg*m²/s²/A" )
    , ("FieldStrength", "kg/s²/A"    )
    , ("Inductance"   , "kg*m²/s²/A²")
    , ("AbsorbedDose" , "m²/s²"      )
    ]

  in fmap concat $ mapM (uncurry makeBase) bases)
-}
