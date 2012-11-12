{-# LANGUAGE TemplateHaskell, LambdaCase, TypeOperators #-}
-- | TemplateHaskell functions for introducing new units.
module Units.TH (u , makeUnit, makeUnits) where

import Prelude hiding (div, exp, Rational)

import Control.Applicative hiding ((<|>))

import Data.Char (toLower)
import qualified Data.Ratio as R
import qualified Data.Map   as M
import Data.Maybe (fromMaybe)

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Units.Internal.Types

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (haskell)
import Text.Parsec.String
import Text.Parsec.Token (parens, naturalOrFloat)

-- Quasiquoter for TString

toTChar :: Char -> Name
toTChar = \case
  'A'->'CA; 'B'->'CB; 'C'->'CC; 'D'->'CD; 'E'->'CE; 'F'->'CF; 'G'->'CG;
  'H'->'CH; 'I'->'CI; 'J'->'CJ; 'K'->'CK; 'L'->'CL; 'M'->'CM; 'N'->'CN;
  'O'->'CO; 'P'->'CP; 'Q'->'CQ; 'R'->'CR; 'S'->'CS; 'T'->'CT; 'U'->'CU;
  'V'->'CV; 'W'->'CW; 'X'->'CX; 'Y'->'CY; 'Z'->'CZ; 'a'->'Ca; 'b'->'Cb;
  'c'->'Cc; 'd'->'Cd; 'e'->'Ce; 'f'->'Cf; 'g'->'Cg; 'h'->'Ch; 'i'->'Ci;
  'j'->'Cj; 'k'->'Ck; 'l'->'Cl; 'm'->'Cm; 'n'->'Cn; 'o'->'Co; 'p'->'Cp;
  'q'->'Cq; 'r'->'Cr; 's'->'Cs; 't'->'Ct; 'u'->'Cu; 'v'->'Cv; 'w'->'Cw;
  'x'->'Cx; 'y'->'Cy; 'z'->'Cz;

  c -> error $ "Unsupported char in TChar literal: " ++ show c

promotedListT :: [Type] -> Type
promotedListT  []    = PromotedNilT
promotedListT (x:xs) = AppT (AppT PromotedConsT x) (promotedListT xs)

-- No longer needed here, but perhaps it may come in handy sometime else
ts :: QuasiQuoter
ts = QuasiQuoter
  (return . ListE . map (ConE . toTChar))
  (return . ListP . map ((`ConP` []) . toTChar))
  (return . promotedListT . map (PromotedT . toTChar))
  undefined

-- Parser and quasiquoters for Unit

data UnitExp = Unit String R.Rational | Mult UnitExp UnitExp | Recip UnitExp

parseUnit :: String -> Maybe UnitExp
parseUnit = either (error . show) id . parse (spaces *> p) "Unit QuasiQuoter"
  where p = optionMaybe unit

unit, name, prim :: Parser UnitExp
unit = buildExpressionParser ops prim
 where
  ops  = [[Infix mult AssocLeft, Infix div AssocLeft]]
  mult =  Mult                  <$ char '*' <* spaces
  div  = (\x -> Mult x . Recip) <$ char '/' <* spaces

prim = name <|> parens haskell unit
name = Unit <$> many1 letter <* spaces <*> option 1 exp
 where
  exp  = char '^' *> spaces *> rational <* spaces
     <|> choice (zipWith (\n o -> n <$ char o) [0..] nums) <* spaces

  nums = ['⁰','¹','²','³','⁴','⁵','⁶','⁷','⁸','⁹']

rational :: Parser R.Rational
rational = neg $ either fromInteger toRational <$> naturalOrFloat haskell
 where
  neg :: Num a => Parser a -> Parser a
  neg p = negate <$ char '-' <*> p <|> p

flatten :: UnitExp -> M.Map String R.Rational
flatten (Unit s r) = M.singleton s r
flatten (Mult a b) = M.unionWith (+) (flatten a) (flatten b)
flatten (Recip  m) = M.map negate (flatten m)

-- Type generation for units

toUnit :: M.Map String R.Rational -> Type
toUnit = promotedListT . map toAssoc . filter ((/=0) . snd) . M.toList

toAssoc :: (String, R.Rational) -> Type
toAssoc (s, r) = AppT (AppT (PromotedT '(:^)) (toTString s)) (toRat r)

toTString :: String -> Type
toTString = promotedListT . map (PromotedT . toTChar)

toRat :: R.Rational -> Type
toRat r = AppT (AppT (PromotedT '(:/)) (toInt a)) (toInt b)
  where (a, b) = (R.numerator r, R.denominator r)

toInt :: Integer -> Type
toInt n
  | n < 0     = AppT (PromotedT 'Neg ) (toNat (abs n - 1))
  | otherwise = AppT (PromotedT 'Norm) (toNat n)

toNat :: Integer -> Type
toNat 0 = PromotedT 'N0
toNat n = AppT (PromotedT 'NS) (toNat (n-1))

quoteUnitT :: String -> Q Type
quoteUnitT = return . p . toUnit . l . fmap flatten . parseUnit
 where
  p = AppT (PromotedT 'EL)
  l = fromMaybe (M.empty)

quoteUnitE :: String -> Q Exp
quoteUnitE s = [| U 1 :: Num a => a :@ $(quoteUnitT s) |]

-- | A QuasiQuoter for units, with the following syntax:
--
--   * A unit itself is a nonempty string of upper case or lower case
--     letters, like ‘kg’ or ‘J’.
--
--   * Units may be combined with *, /, or exponentiated to a rational
--     exponent with ^. The exponent follows the syntactical rules of numeric
--     literals in Haskell, eg. ‘m^2’ or ‘s^0.5’.
--
--   * Alternatively, units can be exponentiated with ‘²’, ‘³’ etc.
--
--   * Units may be surrounded by parentheses, eg. ‘a/(b*c)’
--
--   Most of the time, you'd just want to use a name alone, eg.
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
u = QuasiQuoter quoteUnitE undefined quoteUnitT undefined

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
  t <- [t|Num a => a :@ $(return (ConT n))|]
  b <- [e|U 1|]
  return [ SigD v t, ValD (VarP v) (NormalB b) [] ]
 where
  v = mkName $ uncap (nameBase n)
  uncap  ""    = ""
  uncap (h:xs) = toLower h : xs

-- | Like 'makeUnit' but works on multiple names at once. Provided for
--   convenience.

makeUnits :: [Name] -> Q [Dec]
makeUnits = fmap concat . mapM makeUnit
