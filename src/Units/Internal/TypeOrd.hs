{-# LANGUAGE TemplateHaskell, TypeFamilies, DataKinds, PolyKinds
  , TypeOperators, UndecidableInstances #-}
module Units.Internal.TypeOrd (Compare, makeOrd) where

import Control.Monad (liftM2)
import Language.Haskell.TH

type family Compare (a :: k) (b :: k) :: Ordering

makeOrd :: Name -> Q [Dec]
makeOrd n = do
  TyConI (DataD [] _ [] cs' _) <- reify n
  let cs = zipWith (\(NormalC c []) i -> (c, i)) cs' [0..]
  return $ liftM2 makeCompare cs cs

makeCompare :: (Name, Int) -> (Name, Int) -> Dec
makeCompare (a, i) (b, j) = TySynInstD ''Compare [PromotedT a, PromotedT b] res
 where
  res = case compare i j of
    LT -> PromotedT 'LT
    EQ -> PromotedT 'EQ
    GT -> PromotedT 'GT

-- List comparison

type instance Compare '[] '[] = EQ
type instance Compare (a ': as) (b ': bs) = CmpList (Compare a b) as bs

type family CmpList (o :: Ordering) (a :: [k]) (b :: [k]) :: Ordering
type instance CmpList EQ a b = Compare a b
type instance CmpList LT a b = LT
type instance CmpList GT a b = GT