{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds, TypeOperators #-}
module Units.Metric
  ( module Units.Metric
  , module Units.SI
  ) where

import Units
import Units.SI
import Units.TH

-- Some widely used non-SI units

type Minute    = [u|min |]
type Hour      = [u|h   |]
type Day       = [u|d   |]
type Degree    = [u|deg |]
type ArcMinute = [u|amin|]
type ArcSecond = [u|asec|]

makeUnits [ ''Minute, ''Hour, ''Day, ''Degree, ''ArcMinute, ''ArcSecond ]

type Are     = Hecto * Meter^2
type Hectare = Hecto*Are
type Liter   = (Deci*Meter)^3
type Litre   = Liter
type Gram    = Kilogram/Kilo
type Tonne   = Mega*Gram

makeUnits [ ''Are, ''Hectare, ''Liter, ''Litre, ''Gram, ''Tonne ]

-- Experimentally determined units

type ElectronVolt = [u|eV|]
type AtomicMass   = [u|u |]
type Dalton       = AtomicMass
type AstroUnit    = [u|ua|]
type Charge       = [u|e |]

makeUnits [ ''ElectronVolt, ''AtomicMass, ''Dalton, ''AstroUnit, ''Charge ]

-- Common units not officially sanctioned

type Bar        = Deca^5 * Pascal
type Millibar   = Milli*Bar
type Atmosphere = [u|atm|]

makeUnits [ ''Bar, ''Millibar, ''Atmosphere ]
