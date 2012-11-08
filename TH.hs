{-# LANGUAGE TemplateHaskell, LambdaCase #-}
module Units.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Units.Types

-- Quasi-quoters for TChar

toTCharName :: Char -> Name
toTCharName = \case
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
  (return . ListE . map (ConE . toTCharName))
  (return . ListP . map ((`ConP` []) . toTCharName))
  (return . promotedListT . map (PromotedT . toTCharName))
  (return . const [])
