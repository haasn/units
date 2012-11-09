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

makeUnits [''Meter, ''Kilogram, ''Second, ''Ampere, ''Kelvin, ''Candela]

-- Some derived units

type Newton = Kilogram*Meter/(Second*Second)
makeUnit ''Newton
