{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds, TypeOperators #-}
module Units.Planck where

import Units
import Units.TH

-- Physical constants from which units are derived

type C     = [u|c   |]
type G     = [u|G   |]
type HBar  = [u|hbar|]
type KB    = [u|kB  |]
type FPiE0 = [u|fpez|] -- 4πε₀

makeUnits [ ''C, ''G, ''HBar, ''KB, ''FPiE0 ]

-- Base units

type Length      = Sqrt (HBar * G / C^3)
type Mass        = Sqrt (HBar * C / G)
type Time        = Length / C
type Charge      = Sqrt (FPiE0 * HBar * C)
type Temperature = Mass * C^2 / KB

makeUnits [ ''Length, ''Mass, ''Time, ''Charge, ''Temperature ]

-- Derived units

type Area      = Length^2
type Volume    = Length^3
type Momentum  = Mass * C
type Energy    = Mass * C^2
type Force     = Energy / Length
type Power     = Energy / Time
type Density   = Mass / Volume
type AngFreq   = One / Time
type Pressure  = Force / Area
type Current   = Charge / Time
type Voltage   = Energy / Charge
type Impedance = Voltage / Current

makeUnits
  [ ''Area   , ''Volume  , ''Momentum, ''Energy , ''Force, ''Power, ''Density
  , ''AngFreq, ''Pressure, ''Current , ''Voltage, ''Impedance ]
