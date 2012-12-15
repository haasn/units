{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds, TypeOperators
  , TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}
-- | Units in the metric system (which includes the SI system) as well as
--   some other commonly used units which are not officially sanctioned.
module Units.Metric
  ( module Units.Metric
  , module Units.SI
  ) where

import Units.Prelude
import Units.Convert
import Units.SI

-- Some widely used non-SI units

type Minute    = [u|min |] -- ^ Unit of time, symbol ‘min’
type Hour      = [u|h   |] -- ^ Unit of time, symbol ‘h’
type Day       = [u|d   |] -- ^ Unit of time, symbol ‘d’
type Degree    = [u|deg |] -- ^ Unit of angle, symbol ‘deg’
type ArcMinute = [u|amin|] -- ^ Unit of angle, symbol ‘amin’
type ArcSecond = [u|asec|] -- ^ Unit of angle, symbol ‘asec’

makeUnits [ ''Minute, ''Hour, ''Day, ''Degree, ''ArcMinute, ''ArcSecond ]

type Are     = Hecto * Meter^2 -- ^ Unit of area = 100·m²
type Hectare = Hecto*Are       -- ^ Unit of area = hectoare
type Liter   = (Deci*Meter)^3  -- ^ Unit of volume = (dm)³
type Litre   = Liter           -- ^ British spelling of 'Liter'
type Gram    = Kilogram/Kilo   -- ^ Unit of mass = kg/1000
type Tonne   = Mega*Gram       -- ^ Unit of mass = Mg

makeUnits [ ''Are, ''Hectare, ''Liter, ''Litre, ''Gram, ''Tonne ]

-- Experimentally determined units

type ElectronVolt   = [u|eV|]    -- ^ Unit of energy, symbol ‘eV’
type AtomicMass     = [u|u |]    -- ^ Unit of mass, symbol ‘u’
type Dalton         = AtomicMass -- ^ Alternative name for 'AtomicMass'
type AstroUnit      = [u|ua|]    -- ^ Unit of length, symbol ‘ua’
type ElectronCharge = [u|e |]    -- ^ Unit of charge, symbol ‘e’

makeUnits [ ''ElectronVolt, ''AtomicMass, ''Dalton, ''AstroUnit
          , ''ElectronCharge ]

-- Common units not officially sanctioned

type Bar        = Deca^5 * Pascal -- ^ Unit of pressure = 10^5·Pa
type Millibar   = Milli*Bar       -- ^ Unit of pressure = bar/1000
type Atmosphere = [u|atm|]        -- ^ Unit of pressure, symbol ‘atm’

makeUnits [ ''Bar, ''Millibar, ''Atmosphere ]

-- For testing
type Meter'  = [ts|m|]
type Deca'   = [ts|deca|]
type Second' = [ts|s|]
type Hour'   = [ts|h|]

instance IsoDim Meter' where
  type From Meter' = Meter'
  factor _ = 1

instance IsoDim Deca' where
  type From Deca' = '[]
  factor _ = 10

instance IsoDim Second' where
  type From Second' = Second'
  factor _ = 1

instance IsoDim Hour' where
  type From Hour' = Second'
  factor _ = 3600
