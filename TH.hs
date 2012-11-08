{-# LANGUAGE TemplateHaskell, LambdaCase #-}
module Units.TH (ts, u) where

import Prelude hiding (div, exp)

import Control.Applicative hiding ((<|>))

import qualified Data.Map as M

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Units.Types

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (haskell)
import Text.Parsec.String
import Text.Parsec.Token (parens, natural)

-- Quasiquoters for TChar

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

ts :: QuasiQuoter
ts = QuasiQuoter
  (return . ListE . map (ConE . toTChar))
  (return . ListP . map ((`ConP` []) . toTChar))
  (return . promotedListT . map (PromotedT . toTChar))
  undefined

-- Parser and quasiquoters for Unit

data UnitExp = Unit String Integer | Mult UnitExp UnitExp | Recip UnitExp
  deriving Show

parseUnit :: String -> UnitExp
parseUnit = either (error . show) id . parse (spaces *> unit) "Unit QuasiQuoter"

unit, name, prim :: Parser UnitExp
unit = buildExpressionParser ops prim
 where
  ops  = [[Infix mult AssocLeft, Infix div AssocLeft]]
  mult =  Mult                  <$ char '*' <* spaces
  div  = (\x -> Mult x . Recip) <$ char '/' <* spaces

prim = name <|> parens haskell unit
name = Unit <$> many1 letter <* spaces <*> option 1 exp
  where exp = char '^' *> spaces *> natural haskell <* spaces
          <|> 2 <$ char '²' <* spaces

flatten :: UnitExp -> M.Map String Integer
flatten (Unit s i) = M.singleton s i
flatten (Mult a b) = M.unionWith (+) (flatten a) (flatten b)
flatten (Recip  m) = M.map negate (flatten m)

toUnit :: M.Map String Integer -> Type
toUnit = promotedListT . map toAssoc . filter ((/=0) . snd) . M.toList

toAssoc :: (String, Integer) -> Type
toAssoc (s, i) = AppT (AppT (PromotedT '(:^)) (toTString s)) (toInt i)

toTString :: String -> Type
toTString = promotedListT . map (PromotedT . toTChar)

toInt :: Integer -> Type
toInt n
  | n < 0     = AppT (PromotedT 'Neg ) (toNat (abs n - 1))
  | otherwise = AppT (PromotedT 'Norm) (toNat n)

toNat :: Integer -> Type
toNat 0 = PromotedT 'N0
toNat n = AppT (PromotedT 'NS) (toNat (n-1))

quoteUnitT :: String -> Type
quoteUnitT = AppT (PromotedT 'EL) . toUnit . flatten . parseUnit

u :: QuasiQuoter
u = QuasiQuoter undefined undefined (return . quoteUnitT) undefined
