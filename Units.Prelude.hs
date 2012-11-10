{-# LANGUAGE FlexibleInstances, TypeFamilies, TypeOperators, DataKinds #-}
module Units.Prelude
  ( (Units.Prelude.+)
  , (Units.Prelude.-)
  , (Units.Prelude.*)
  , (Units.Prelude./)
  ) where

import Data.Singletons
import Units
import Units.Types

(+) :: Num a => a :@ u -> a :@ u -> a :@ u
(+) = addU
infixl 6 +

(-) :: Num a => a :@ u -> a :@ u -> a :@ u
(-) = subU
infixl 6 -

(*) :: Num a => a :@ u -> a :@ v -> a :@ u*v
(*) = mulU
infixl 7 *

(/) :: Fractional a => a :@ u -> a :@ v -> a :@ u/v
(/) = divU
infixl 7 /

instance (Num a, u ~ One) => Num (a :@ u) where
  fromInteger = U . fromInteger

  (+)    = error "Use Units.Prelude.+"
  (*)    = error "Use Units.Prelude.*"
  abs    = lit . abs . unU
  signum = lit . abs . unU

instance (Fractional a, u ~ One) => Fractional (a :@ u) where
  fromRational = U . fromRational

  (/)   = error "Use Units.Prelude./"
  recip = error "Use Units.recip"
