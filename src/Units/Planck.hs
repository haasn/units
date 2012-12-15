{-
{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds, TypeOperators #-}
-- | Units of measure in the Planck system, which normalizes key physical
--   constants to ‘1’ and derives all units from those.
module Units.Planck where

import Units.Prelude

-- Physical constants from which units are derived

type C     = [u|c   |] -- ^ Speed of light in vacuum, symbol ‘c’
type G     = [u|G   |] -- ^ Gravitational constant, symbol ‘G’
type HBar  = [u|hbar|] -- ^ Reduced Planck constant, symbol ‘hbar’
type KB    = [u|kB  |] -- ^ Boltzmann constant, symbol ‘kB’
type FPiE0 = [u|fpez|] -- ^ Coulomb constant, symbol ‘fpez’ = 4πε₀

makeUnits [ ''C, ''G, ''HBar, ''KB, ''FPiE0 ]

-- Base units

type Length      = Sqrt (HBar * G / C^3)   -- ^ = √(ħ·G/c³)
type Mass        = Sqrt (HBar * C / G)     -- ^ = √(ħ·c/G)
type Time        = Length / C              -- ^ = √(ħ·G/c⁵)
type Charge      = Sqrt (FPiE0 * HBar * C) -- ^ = √(4πε₀·ħ·c)
type Temperature = Mass * C^2 / KB         -- ^ = √(ħ·c⁵/(G·kB²))

makeUnits [ ''Length, ''Mass, ''Time, ''Charge, ''Temperature ]

-- Derived units

type Area      = Length^2          -- ^ = ħ·G/c³
type Volume    = Length^3          -- ^ = √((ħ·G)³/c⁹)
type Momentum  = Mass * C          -- ^ = √(ħ·c³/G)
type Energy    = Mass * C^2        -- ^ = √(ħ·c⁵/G)
type Force     = Energy / Length   -- ^ = c⁴/G
type Power     = Energy / Time     -- ^ = c⁵/G
type Density   = Mass / Volume     -- ^ = c⁵/(ħ·G²)
type AngFreq   = One / Time        -- ^ = √(c⁵/(ħ·G))
type Pressure  = Force / Area      -- ^ = c⁷/(ħ·G²)
type Current   = Charge / Time     -- ^ = √(c⁶4πε₀/G)
type Voltage   = Energy / Charge   -- ^ = √(c⁴/(G·4πε₀))
type Impedance = Voltage / Current -- ^ = 1/(4πε₀)

makeUnits
  [ ''Area   , ''Volume  , ''Momentum, ''Energy , ''Force, ''Power, ''Density
  , ''AngFreq, ''Pressure, ''Current , ''Voltage, ''Impedance ]
-}
