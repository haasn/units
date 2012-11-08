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

meter :: Num a => a :@ Meter
meter = [u|m|]

kilogram :: Num a => a :@ Kilogram
kilogram = [u|kg|]

second :: Num a => a :@ Second
second = [u|s|]

ampere :: Num a => a :@ Ampere
ampere = [u|A|]

kelvin :: Num a => a :@ Kelvin
kelvin = [u|K|]

candela :: Num a => a :@ Candela
candela = [u|cd|]
