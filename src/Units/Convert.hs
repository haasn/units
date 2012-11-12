{-# LANGUAGE ConstraintKinds, TypeFamilies, TypeOperators, DataKinds
  , FlexibleInstances, TemplateHaskell, TypeSynonymInstances #-}
module Units.Convert where

import Units.Internal.Types
import GHC.Exts (Constraint)

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Units

class IsoUnit (u :: Unit) where
  type Base u :: Unit
  type Ctx  u :: * -> Constraint

  toBase   :: Ctx u a => a :@ u -> a :@ Base u
  fromBase :: Ctx u a => a :@ Base u -> a :@ u

type Convert u j a = (IsoUnit u, IsoUnit j, Base u ~ Base j, Ctx u a, Ctx j a)

convert :: Convert u j a => a :@ u -> a :@ j
convert = fromBase . toBase

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
