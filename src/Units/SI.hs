{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds, TypeOperators
  , TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses
  , TypeFamilies #-}
-- | An implementation of the base and derived SI units, as well as
--   numeric prefixes.
module Units.SI where

import Units
import Units.TH
import qualified Units.Prelude as U

-- SI base units

type Meter    = U "m"  -- ^ Unit of distance, symbol ‘m’
type Kilogram = U "kg" -- ^ Unit of weight, symbol ‘kg’
type Second   = U "s"  -- ^ Unit of time, symbol ‘s’
type Ampere   = U "A"  -- ^ Unit of current, symbol ‘A’
type Kelvin   = U "K"  -- ^ Unit of temperature, symbol ‘K’
type Candela  = U "cd" -- ^ Unit of luminosity, symbol ‘cd’

makeUnits [ ''Meter, ''Kilogram, ''Second, ''Ampere, ''Kelvin, ''Candela ]

-- Some derived units

type Hertz   = One / Second     -- ^ Unit of frequency = 1/s
type Newton  = Kilogram * Meter / Second^2 -- ^ Unit of force = kg·m/s²
type Pascal  = Newton / Meter^2 -- ^ Unit of pressure = N/m²
type Joule   = Newton * Meter   -- ^ Unit of energy = N·m
type Watt    = Joule / Second   -- ^ Unit of power = J/s
type Coulomb = Ampere * Second  -- ^ Unit of electric charge = A·s
type Volt    = Watt / Ampere    -- ^ Unit of electric potential = W/A
type Farad   = Coulomb / Volt   -- ^ Unit of capacitance = C/V
type Ohm     = Volt / Ampere    -- ^ Unit of resistance = V/A
type Siemens = One / Ohm        -- ^ Unit of conductance = A/V
type Weber   = Joule / Ampere   -- ^ Unit of magnetic flux = J/A
type Tesla   = Weber / Meter^2  -- ^ Unit of magnetic field strength = Wb/m²
type Henry   = Weber / Ampere   -- ^ Unit of inductance = Wb/A
type Gray    = Joule / Kilogram -- ^ Unit of absorbed radiation = J/kg

makeUnits [ ''Hertz, ''Newton, ''Pascal , ''Joule, ''Watt , ''Coulomb, ''Volt,
            ''Farad, ''Ohm   , ''Siemens, ''Weber, ''Tesla, ''Henry ]

-- Prefixes are carried along to keep track of the base

type Deca  = U "deca" -- ^ Base unit of multiplication by 10, symbol ‘deca’
type Hecto = Deca^2   -- ^ 100 = 10^2
type Kilo  = Deca^3   -- ^ 1000 = 10^3
type Mega  = Kilo^2   -- ^ 1000^2 = 10^6
type Giga  = Kilo^3   -- ^ 1000^3 = 10^9
type Tera  = Kilo^4   -- ^ 1000^4 = 10^12
type Peta  = Kilo^5   -- ^ 1000^5 = 10^15
type Exa   = Kilo^6   -- ^ 1000^6 = 10^18
type Zetta = Kilo^7   -- ^ 1000^7 = 10^21
type Yotta = Kilo^8   -- ^ 1000^8 = 10^24

makeUnits [ ''Deca , ''Hecto, ''Kilo, ''Mega, ''Giga, ''Tera, ''Peta, ''Exa
          , ''Zetta, ''Yotta ]

type Deci  = One / Deca -- ^ 1/deca = 10^-1
type Centi = Deci^2     -- ^ 1/100 = 10^-2
type Cent  = Centi      -- ^ Renamed version of Centi, eg. for currencies
type Milli = Deci^3     -- ^ 1/1000 = 10^-3
type Micro = Milli^2    -- ^ 1000^-2 = 10^-6
type Nano  = Milli^3    -- ^ 1000^-3 = 10^-9
type Pico  = Milli^4    -- ^ 1000^-4 = 10^-12
type Femto = Milli^5    -- ^ 1000^-5 = 10^-15
type Atto  = Milli^6    -- ^ 1000^-6 = 10^-18
type Zepto = Milli^7    -- ^ 1000^-7 = 10^-21
type Yocto = Milli^8    -- ^ 1000^-8 = 10^-24

makeUnits [ ''Deci , ''Centi, ''Cent , ''Milli, ''Micro, ''Nano, ''Pico
          , ''Femto, ''Atto , ''Zepto, ''Yocto ]

-- These are technically equivalent to some existing units but are kept
-- separate because they have different semantics

type Mole       = U "mol" -- ^ Unit of amount of substance, symbol ‘mol’
type Radian     = U "rad" -- ^ Unit of angle, symbol ‘rad’
type Steradian  = U "sr"  -- ^ Unit of solid angle, symbol ‘sr’
type Celsius    = U "C"   -- ^ Relative unit of temperature, symbol 'C'
type Becquerell = U "Bq"  -- ^ Unit of radioacitivity, symbol ‘Bq’
type Sievert    = U "Sv"  -- ^ Unit of equivalent dosem symbol ‘Sv’

makeUnits [ ''Mole, ''Radian, ''Steradian, ''Celsius, ''Becquerell, ''Sievert ]

-- Conversions function from celsius to kelvin

celsiusKelvin :: Fractional a => a :@ Celsius -> a :@ Kelvin
celsiusKelvin x = (x U.- 273.15 U.* celsius) U./ celsius U.* kelvin

type Lumen = Candela * Steradian -- ^ Unit of luminous flux = cd·sr
type Lux   = Lumen / Meter^2     -- ^ Unit of illuminance = lm/m²
type Katal = Mole / Second       -- ^ Unit of catalytic activity = mol/s

makeUnits [ ''Lumen, ''Lux, ''Katal ]

-- Conversions on all base units

makeConvert ''Meter     ''Meter    1
makeConvert ''Kilogram  ''Kilogram 1
makeConvert ''Second    ''Second   1
makeConvert ''Ampere    ''Ampere   1
makeConvert ''Kelvin    ''Kelvin   1
makeConvert ''Candela   ''Candela  1
makeConvert ''Deca      ''One      10
makeConvert ''Mole      ''One      6.0221417930e23
makeConvert ''Radian    ''One      (1/(2*pi))
makeConvert ''Steradian ''One      (1/(4*pi))
