{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds, TypeOperators #-}
module Units.SI where

import Units
import Units.TH

-- SI base units

type Meter    = [u|m |]
type Kilogram = [u|kg|]
type Second   = [u|s |]
type Ampere   = [u|A |]
type Kelvin   = [u|K |]
type Candela  = [u|cd|]

makeUnits [ ''Meter, ''Kilogram, ''Second, ''Ampere, ''Kelvin, ''Candela ]

-- Some derived units

type Hertz   = One / Second
type Newton  = Kilogram * Meter / Second^2
type Pascal  = Newton / Meter^2
type Joule   = Newton * Meter
type Watt    = Joule / Second
type Coulomb = Ampere * Second
type Volt    = Watt / Ampere
type Farad   = Coulomb / Volt
type Ohm     = Volt / Ampere
type Siemens = One / Ohm
type Weber   = Joule / Ampere
type Tesla   = Weber / Meter^2
type Henry   = Weber / Ampere
type Gray    = Joule / Kilogram

makeUnits [ ''Hertz, ''Newton, ''Pascal , ''Joule, ''Watt , ''Coulomb, ''Volt,
            ''Farad, ''Ohm   , ''Siemens, ''Weber, ''Tesla, ''Henry ]

-- Prefixes are carried along to keep track of the base

type Deca  = [u|deca|]
type Hecto = Deca^2
type Kilo  = Deca^3
type Mega  = Kilo^2
type Giga  = Kilo^3
type Tera  = Kilo^4
type Peta  = Kilo^5
type Exa   = Kilo^6
type Zetta = Kilo^7
type Yotta = Kilo^8

makeUnits [ ''Deca , ''Hecto, ''Kilo, ''Mega, ''Giga, ''Tera, ''Peta, ''Exa
          , ''Zetta, ''Yotta ]

type Deci  = One / Deca
type Centi = Deci^2
type Milli = Deci^3
type Micro = Milli^2
type Nano  = Milli^3
type Pico  = Milli^4
type Femto = Milli^5
type Atto  = Milli^6
type Zepto = Milli^7
type Yocto = Milli^8

makeUnits [ ''Deci , ''Centi, ''Milli, ''Micro, ''Nano, ''Pico, ''Femto, ''Atto
          , ''Zepto, ''Yocto ]

-- These are technically equivalent to some existing units but are kept
-- separate because they have different semantics

type Mole       = [u|mol|]
type Radian     = [u|rad|]
type Steradian  = [u|sr |]
type Celsius    = [u|dC |]
type Becquerell = [u|Bq |]
type Sievert    = [u|Sv |]

makeUnits [ ''Mole, ''Radian, ''Steradian, ''Celsius, ''Becquerell, ''Sievert ]

type Lumen = Candela * Steradian
type Lux   = Lumen / Meter^2
type Katal = Mole / Second

makeUnits [ ''Lumen, ''Lux, ''Katal ]
