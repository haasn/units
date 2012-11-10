{-# LANGUAGE FlexibleInstances, TypeFamilies, TypeOperators, DataKinds #-}
-- | Functions to replace the numerical functions from the Prelude.
--   See "Units" for the documentation of '(+)', '(-)', '(*)' and '(/)'.
--
--   This module includes Num and Fractional instances for '(:@)', which
--   implement 'fromInteger' and 'fromRational', to avoid having to use 'lit'
--   on every single numeric literal. This allows, for example:
--
--   > 5 * meter

module Units.Prelude
  ( (Units.Prelude.+)
  , (Units.Prelude.-)
  , (Units.Prelude.*)
  , (Units.Prelude./)
  ) where

import Units

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
  fromInteger = lit . fromInteger

  (+)    = error "Use Units.Prelude.+"
  (*)    = error "Use Units.Prelude.*"
  abs    = lit . abs . unTag
  signum = lit . abs . unTag

instance (Fractional a, u ~ One) => Fractional (a :@ u) where
  fromRational = lit . fromRational

  (/)   = error "Use Units.Prelude./"
  recip = error "Use Units.recip"
