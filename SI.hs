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
type Newton  = Kilogram * Meter / (Second*Second)
type Pascal  = Newton / (Meter*Meter)
type Joule   = Newton * Meter
type Watt    = Joule / Second
type Coulomb = Ampere * Second
type Volt    = Watt / Ampere
type Farad   = Coulomb / Volt
type Ohm     = Volt / Ampere
type Siemens = One / Ohm
type Weber   = Joule / Ampere
type Tesla   = Weber / (Meter*Meter)
type Henry   = Weber / Ampere
type Gray    = Joule / Kilogram

makeUnits [ ''Hertz, ''Newton, ''Pascal , ''Joule, ''Watt , ''Coulomb, ''Volt,
            ''Farad, ''Ohm   , ''Siemens, ''Weber, ''Tesla, ''Henry ]

-- These are technically equivalent to some existing units but are kept
-- separate because they have different semantics

type Mole       = [u|mol|]
type Radian     = [u|rad|]
type Steradian  = [u|sr|]
type Celsius    = [u|dC|]
type Becquerell = [u|Bq|]
type Sievert    = [u|Sv|]

makeUnits [ ''Mole, ''Radian, ''Steradian, ''Celsius, ''Becquerell, ''Sievert ]

type Lumen = Candela * Steradian
type Lux   = Lumen / (Meter*Meter)
type Katal = Mole / Second

makeUnits [ ''Lumen, ''Lux, ''Katal ]
