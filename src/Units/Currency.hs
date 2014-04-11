{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds, TypeOperators #-}
-- | Units for commonly used currencies
module Units.Currency where

import Units.Prelude
import Units.SI (Cent)

-- Full list of currency names by ISO codes

type EUR = U "EUR" -- ^ European euro
type USD = U "USD" -- ^ United States dollar
type CNY = U "CNY" -- ^ Chinese yuan
type JPY = U "JPY" -- ^ Japanese yen
type GBP = U "GBP" -- ^ British pound
type BRL = U "BRL" -- ^ Brazilean real
type INR = U "INR" -- ^ Indian rupee
type CAD = U "CAD" -- ^ Canadian dollar
type RUB = U "RUB" -- ^ Russian ruble
type AUD = U "AUD" -- ^ Australian dollar
type MXN = U "MXN" -- ^ Mexican peso
type KRW = U "KRW" -- ^ South Korean won
type TRY = U "TRY" -- ^ Turkish lira
type IDR = U "IDR" -- ^ Indonesian rupiah
type CHF = U "CHF" -- ^ Swiss franc
type PLN = U "PLN" -- ^ Polish złoty
type SEK = U "SEK" -- ^ Swedish krona
type SAR = U "SAR" -- ^ Saudi riyal
type NOK = U "NOK" -- ^ Norwegian krone
type VEF = U "VEF" -- ^ Venezuelan bolívar
type IRR = U "IRR" -- ^ Iranian rial
type ARS = U "ARS" -- ^ Argentine peso
type ZAR = U "ZAR" -- ^ South African rand
type THB = U "THB" -- ^ Thai baht
type DKK = U "DKK" -- ^ Danish krone
type AED = U "AED" -- ^ United Arab Emirates dirham
type COP = U "COP" -- ^ Colombian peso
type MYR = U "MYR" -- ^ Malaysian ringgit
type HKD = U "HKD" -- ^ Hong Kong dollar
type BND = U "BND" -- ^ Brunei dollar
type SGD = U "SGD" -- ^ Singapore dollar
type ILS = U "ILS" -- ^ Israeli new shekel
type EGP = U "EGP" -- ^ Egyptian pound
type CLP = U "CLP" -- ^ Chilean peso
type PHP = U "PHP" -- ^ Philippine peso
type CZK = U "CZK" -- ^ Czech koruna
type NGN = U "NGN" -- ^ Nigerian naira
type PKR = U "PKR" -- ^ Pakistani rupee
type RON = U "RON" -- ^ Romanian leu
type DZD = U "DZD" -- ^ Algerian dinar
type PEN = U "PEN" -- ^ Peruvian neuvo sol
type KZT = U "KZT" -- ^ Kazakhstani tenge
type NZD = U "NZD" -- ^ New Zealand dollar
type UAH = U "UAH" -- ^ Ukrainian hryvnia
type HUF = U "HUF" -- ^ Hungarian forint
type QAR = U "QAR" -- ^ Qatari riyal
type KWD = U "KWD" -- ^ Kuwaiti dinar
type VND = U "VND" -- ^ Vietnamese đồng
type BDT = U "BDT" -- ^ Bangladeshi taka
type MAD = U "MAD" -- ^ Moroccan dirham
type AOA = U "AOA" -- ^ Angolan kwanza
type SDG = U "SDG" -- ^ Sudanese pound
type LYD = U "LYD" -- ^ Libyan dinar
type CUC = U "CUC" -- ^ Cuban convertible peso
type CUP = U "CUP" -- ^ Cuban peso
type HRK = U "HRK" -- ^ Croatian kuna
type SYP = U "SYP" -- ^ Syrian pound
type OMR = U "OMR" -- ^ Omani rial
type BYR = U "BYR" -- ^ Belarusian ruble
type AZN = U "AZN" -- ^ Azerbaijani manat
type DOP = U "DOP" -- ^ Dominican peso
type LKR = U "LKR" -- ^ Sri Lankan rupee
type BGN = U "BGN" -- ^ Bulgarian lev
type TND = U "TND" -- ^ Tunisian dinar
type MMK = U "MMK" -- ^ Burmese kyat
type GTQ = U "GTQ" -- ^ Guatemalan quetzal
type UYU = U "UYU" -- ^ Uruguayan peso
type LBP = U "LBP" -- ^ Lebanese pound
type UZS = U "UZS" -- ^ Uzbekistani som
type RSD = U "RSD" -- ^ Serbian dinar
type LTL = U "LTL" -- ^ Lithuanian litas
type CRC = U "CRC" -- ^ Costa Rican colón
type YER = U "YER" -- ^ Yemeni rial
type GHS = U "GHS" -- ^ Ghana cedi
type KES = U "KES" -- ^ Kenyan shilling
type IQD = U "IQD" -- ^ Iraqi dinar
type JOD = U "JOD" -- ^ Jordanian dinar
type MOP = U "MOP" -- ^ Macanese pataca
type ETB = U "ETB" -- ^ Ethiopian birr
type PAB = U "PAB" -- ^ Panamanian balboa
type LVL = U "LVL" -- ^ Latvian lats
type XAF = U "XAF" -- ^ Central African CFA franc
type TMT = U "TMT" -- ^ Turkmenistan manat
type BHD = U "BHD" -- ^ Bahraini dinar
type XOF = U "XOF" -- ^ West African CFA franc
type TZS = U "TZS" -- ^ Tanzanian shilling
type SVC = U "SVC" -- ^ Salvadoran colón
type TTD = U "TTD" -- ^ Trinidad and Tobago dollar
type BOB = U "BOB" -- ^ Bolivian boliviano

type BTC = U "BTC" -- ^ Bitcoin

makeUnits
 [ ''EUR, ''USD, ''CNY, ''JPY, ''GBP, ''BRL, ''INR, ''CAD, ''RUB, ''AUD, ''MXN
 , ''KRW, ''TRY, ''IDR, ''CHF, ''PLN, ''SEK, ''SAR, ''NOK, ''VEF, ''IRR, ''ARS
 , ''ZAR, ''THB, ''DKK, ''AED, ''COP, ''MYR, ''HKD, ''BND, ''SGD, ''ILS, ''EGP
 , ''CLP, ''PHP, ''CZK, ''NGN, ''PKR, ''RON, ''DZD, ''PEN, ''KZT, ''NZD, ''UAH
 , ''HUF, ''QAR, ''KWD, ''VND, ''BDT, ''MAD, ''AOA, ''SDG, ''LYD, ''CUC, ''CUP
 , ''HRK, ''SYP, ''OMR, ''BYR, ''AZN, ''DOP, ''LKR, ''BGN, ''TND, ''MMK, ''GTQ
 , ''UYU, ''LBP, ''UZS, ''RSD, ''LTL, ''CRC, ''YER, ''GHS, ''KES, ''IQD, ''JOD
 , ''MOP, ''ETB, ''PAB, ''LVL, ''XAF, ''TMT, ''BHD, ''XOF, ''TZS, ''SVC, ''TTD
 , ''BOB, ''BTC
 ]

-- More familiar names for the most popular currencies

type Euro    = EUR
type Dollar  = USD
type Yuan    = CNY
type Yen     = JPY
type Pound   = GBP
type Bitcoin = BTC

makeUnits [ ''Euro, ''Dollar, ''Yuan, ''Yen, ''Pound, ''Bitcoin ]

-- Renamed versions of subdivisions

type EuroCent = Euro*Cent
type USCent   = USD *Cent
type Fen      = Yuan*Cent
type Sen      = Yen *Cent
type Penny    = GBP *Cent
type Bitcent  = BTC *Cent

makeUnits [ ''EuroCent, ''USCent, ''Fen, ''Sen, ''Penny, ''Bitcent ]

