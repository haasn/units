{-# LANGUAGE TemplateHaskell, LambdaCase, TypeOperators #-}
-- | TemplateHaskell functions for introducing new units.
module Units.TH (u, makeUnit, makeUnits, makeConvert) where

import Prelude hiding (div, exp, Rational, Int)

import Data.Char (toLower)
import Data.Maybe (fromMaybe, catMaybes)

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Units.Convert
import Units.Internal.Types

-- Quasiquoter for TString

toTChar' :: Char -> Name
toTChar' = \case
  'A'->'CA; 'B'->'CB; 'C'->'CC; 'D'->'CD; 'E'->'CE; 'F'->'CF; 'G'->'CG;
  'H'->'CH; 'I'->'CI; 'J'->'CJ; 'K'->'CK; 'L'->'CL; 'M'->'CM; 'N'->'CN;
  'O'->'CO; 'P'->'CP; 'Q'->'CQ; 'R'->'CR; 'S'->'CS; 'T'->'CT; 'U'->'CU;
  'V'->'CV; 'W'->'CW; 'X'->'CX; 'Y'->'CY; 'Z'->'CZ; 'a'->'Ca; 'b'->'Cb;
  'c'->'Cc; 'd'->'Cd; 'e'->'Ce; 'f'->'Cf; 'g'->'Cg; 'h'->'Ch; 'i'->'Ci;
  'j'->'Cj; 'k'->'Ck; 'l'->'Cl; 'm'->'Cm; 'n'->'Cn; 'o'->'Co; 'p'->'Cp;
  'q'->'Cq; 'r'->'Cr; 's'->'Cs; 't'->'Ct; 'u'->'Cu; 'v'->'Cv; 'w'->'Cw;
  'x'->'Cx; 'y'->'Cy; 'z'->'Cz;

  c -> error $ "Unsupported char in TChar literal: " ++ show c

toTChar :: Char -> Maybe Name
toTChar c | c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' = Just (toTChar' c)
          | otherwise = Nothing

toTString :: String -> Type
toTString = promotedListT . map PromotedT . catMaybes . map toTChar

promotedListT :: [Type] -> Type
promotedListT  []    = PromotedNilT
promotedListT (x:xs) = AppT (AppT PromotedConsT x) (promotedListT xs)

-- Parser and quasiquoters for Unit

quoteUnitT :: String -> Q Type
quoteUnitT s = return $ AppT (PromotedT 'EL)
                (promotedListT [AppT (AppT (PromotedT '(:^)) ts) (ConT ''I1)])
  where ts = toTString s

-- | A QuasiQuoter for units. Only alphanumeric characters are used.
--
--   > type Meter = [u|m|]
--
--   Note that these units live in the same namespace:
--
--   > type Foo = [u|abc|]
--   > type Bar = [u|abc|]
--
--   ‘Foo’ and ‘Bar’ refer to the same unit, that is, Foo ~ Bar

u :: QuasiQuoter
u = QuasiQuoter undefined undefined quoteUnitT undefined

-- Demote a unit to the value level

-- | A TemplateHaskell function for constructing the value-level units that
--   correspond to each unit type. You want to use this for each named unit
--   introduced.
--
--   @
--     type Foo = [u|foo|]
--     makeUnit ''Foo
--   @
--
--   In this example, 'makeUnit' generates a value ‘foo :: Num a => a :\@ Foo’.
--   The name used is just the type name with the first letter made lower case.

makeUnit :: Name -> Q [Dec]
makeUnit n = do
  let v            = mkName $ uncap (nameBase n)
      uncap  ""    = ""
      uncap (h:xs) = toLower h : xs

  t <- [t|Num a => a :@ $(return (ConT n))|]
  b <- [e|U 1|]
  return [ SigD v t, ValD (VarP v) (NormalB b) [] ]

-- | Like 'makeUnit' but works on multiple names at once. Provided for
--   convenience.

makeUnits :: [Name] -> Q [Dec]
makeUnits = fmap concat . mapM makeUnit

-- | Generate an isomorphism to some base unit

makeConvert :: Real a => Name -> Name -> a -> Q [Dec]
makeConvert un bn f = do
  TyConI (TySynD _ [] ut) <- reify un
  let ud = getDim ut

  return [ InstanceD [] (AppT (ConT ''IsoDim) ud)
    [ TySynInstD ''From [ud] (ConT bn)
    , FunD 'factor [Clause [WildP]
        (NormalB (LitE (RationalL (toRational f)))) []]
    ]]

getDim :: Type -> Type
getDim (AppT (ConT el) (AppT (AppT PromotedConsT
  (AppT (AppT (ConT (^)) n) (ConT i1))) PromotedNilT)) = n

getDim t = error $ "Units.TH.getDim: Not a valid (base) unit: " ++ show t
