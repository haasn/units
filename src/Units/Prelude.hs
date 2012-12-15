{-# LANGUAGE FlexibleInstances, TypeFamilies, TypeOperators, DataKinds #-}
-- | Functions to replace the numerical functions from the Prelude.
--   See "Units" for the documentation of '(+)', '(-)', '(*)' and '(/)'.
--
--   This module includes Num, Fractional, Floating instances for '(:@)', which
--   implement 'fromInteger' and 'fromRational', to avoid having to use 'lit'
--   on every single numeric literal. This allows, for example:
--
--   > 5 * meter
--
--   It also means you can directly work with dimensionless quantities as with
--   regular numbers without having to constantly untag/retag them.

module Units.Prelude
  ( module Units
  , module Units.TH

  , (Units.Prelude.+)
  , (Units.Prelude.-)
  , (Units.Prelude.*)
  , (Units.Prelude./)

  , Convert, convert
  ) where

import Units
import Units.TH
import Units.Convert

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
  (+)    = addU
  (*)    = mulU
  abs    = lit . abs . unTag
  signum = lit . abs . unTag

instance (Fractional a, u ~ One) => Fractional (a :@ u) where
  fromRational = lit . fromRational
  (/)   = divU
  recip = lit . recip . unTag

instance (Floating a, u ~ One) => Floating (a :@ u) where
  pi    = lit pi
  a ** b      = lit $ unTag a ** unTag b
  logBase a b = lit $ unTag a `logBase` unTag b

  exp   = lit . exp   . unTag
  sqrt  = lit . sqrt  . unTag
  log   = lit . log   . unTag
  sin   = lit . sin   . unTag
  tan   = lit . tan   . unTag
  cos   = lit . cos   . unTag
  asin  = lit . asin  . unTag
  atan  = lit . atan  . unTag
  acos  = lit . acos  . unTag
  sinh  = lit . sinh  . unTag
  tanh  = lit . tanh  . unTag
  cosh  = lit . cosh  . unTag
  asinh = lit . asinh . unTag
  atanh = lit . atanh . unTag
  acosh = lit . acosh . unTag
