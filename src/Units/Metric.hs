{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds, TypeOperators
  , TypeSynonymInstances, FlexibleInstances, TypeFamilies
  , UndecidableInstances #-}
-- | Units in the metric system (which includes the SI system) as well as
--   some other commonly used units which are not officially sanctioned.
module Units.Metric
  ( module Units.Metric
  , module Units.SI
  ) where

import Units
import Units.TH
import Units.SI

-- Some widely used non-SI units

type Minute    = U "min"  -- ^ Unit of time, symbol ‘min’
type Hour      = U "h"    -- ^ Unit of time, symbol ‘h’
type Day       = U "d"    -- ^ Unit of time, symbol ‘d’
type Degree    = U "deg"  -- ^ Unit of angle, symbol ‘deg’
type ArcMinute = U "amin" -- ^ Unit of angle, symbol ‘amin’
type ArcSecond = U "asec" -- ^ Unit of angle, symbol ‘asec’

makeUnits [ ''Minute, ''Hour, ''Day, ''Degree, ''ArcMinute, ''ArcSecond ]

type Are     = Hecto * Meter^2 -- ^ Unit of area = 100·m²
type Hectare = Hecto*Are       -- ^ Unit of area = hectoare
type Liter   = (Deci*Meter)^3  -- ^ Unit of volume = (dm)³
type Litre   = Liter           -- ^ British spelling of 'Liter'
type Gram    = Kilogram/Kilo   -- ^ Unit of mass = kg/1000
type Tonne   = Mega*Gram       -- ^ Unit of mass = Mg

makeUnits [ ''Are, ''Hectare, ''Liter, ''Litre, ''Gram, ''Tonne ]

-- Experimentally determined units

type ElectronVolt   = ElectronCharge * Volt -- ^ Unit of energy, symbol ‘eV’

type AtomicMass     = U "u"      -- ^ Unit of mass, symbol ‘u’
type Dalton         = AtomicMass -- ^ Alternative name for 'AtomicMass'
type AstroUnit      = U "au"     -- ^ Unit of length, symbol ‘au’
type ElectronCharge = U "e"      -- ^ Unit of charge, symbol ‘e’

makeUnits [ ''ElectronVolt, ''AtomicMass, ''Dalton, ''AstroUnit
          , ''ElectronCharge ]

-- Common units not officially sanctioned

type Bar        = Deca^5 * Pascal -- ^ Unit of pressure = 10^5·Pa
type Millibar   = Milli*Bar       -- ^ Unit of pressure = bar/1000
type Atmosphere = U "atm"         -- ^ Unit of pressure, symbol ‘atm’

makeUnits [ ''Bar, ''Millibar, ''Atmosphere ]

-- Conversion rules for base units

makeConvert ''Minute         ''Second   60
makeConvert ''Hour           ''Second   (60*60)
makeConvert ''Day            ''Second   (24*60*60)
makeConvert ''Degree         ''One      (1/360)
makeConvert ''ArcMinute      ''One      (1/(360*60))
makeConvert ''ArcSecond      ''One      (1/(360*60*60))
makeConvert ''AtomicMass     ''Kilogram 1.66053892173e-27
makeConvert ''AstroUnit      ''Meter    149597870700
makeConvert ''ElectronCharge ''Coulomb  1.60217656535e-19
makeConvert ''Atmosphere     ''Pascal   1.01325e5
