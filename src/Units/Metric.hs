{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds, TypeOperators #-}
-- | Units in the metric system (which includes the SI system) as well as
--   some other commonly used units which are not officially sanctioned.
module Units.Metric
  ( module Units.Metric
  , module Units.SI
  ) where

import Units
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
