{-# LANGUAGE MultiParamTypeClasses, ConstraintKinds, TypeFamilies #-}
module Units.Convert where

import Units.Internal.Types ((:@))
import GHC.Exts (Constraint)

class Convert a b where
  type C a b :: * -> Constraint

  convert :: C a b n => n :@ a -> n :@ b
