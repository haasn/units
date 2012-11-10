{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds, TypeOperators #-}
-- | Units for commonly used currencies
module Units.Currency where

import Units
import Units.SI (Cent)

-- Full list of currency names by ISO codes

type EUR = [u|EUR|] -- ^ European euro
type USD = [u|USD|] -- ^ United States dollar
type CNY = [u|CNY|] -- ^ Chinese yuan
type JPY = [u|JPY|] -- ^ Japanese yen
type GBP = [u|GBP|] -- ^ British pound
type BRL = [u|BRL|] -- ^ Brazilean real
type INR = [u|INR|] -- ^ Indian rupee
type CAD = [u|CAD|] -- ^ Canadian dollar
type RUB = [u|RUB|] -- ^ Russian ruble
type AUD = [u|AUD|] -- ^ Australian dollar
type MXN = [u|MXN|] -- ^ Mexican peso
type KRW = [u|KRW|] -- ^ South Korean won
type TRY = [u|TRY|] -- ^ Turkish lira
type IDR = [u|IDR|] -- ^ Indonesian rupiah
type CHF = [u|CHF|] -- ^ Swiss franc
type PLN = [u|PLN|] -- ^ Polish złoty
type SEK = [u|SEK|] -- ^ Swedish krona
type SAR = [u|SAR|] -- ^ Saudi riyal
type NOK = [u|NOK|] -- ^ Norwegian krone
type VEF = [u|VEF|] -- ^ Venezuelan bolívar
type IRR = [u|IRR|] -- ^ Iranian rial
type ARS = [u|ARS|] -- ^ Argentine peso
type ZAR = [u|ZAR|] -- ^ South African rand
type THB = [u|THB|] -- ^ Thai baht
type DKK = [u|DKK|] -- ^ Danish krone
type AED = [u|AED|] -- ^ United Arab Emirates dirham
type COP = [u|COP|] -- ^ Colombian peso
type MYR = [u|MYR|] -- ^ Malaysian ringgit
type HKD = [u|HKD|] -- ^ Hong Kong dollar
type BND = [u|BND|] -- ^ Brunei dollar
type SGD = [u|SGD|] -- ^ Singapore dollar
type ILS = [u|ILS|] -- ^ Israeli new shekel
type EGP = [u|EGP|] -- ^ Egyptian pound
type CLP = [u|CLP|] -- ^ Chilean peso
type PHP = [u|PHP|] -- ^ Philippine peso
type CZK = [u|CZK|] -- ^ Czech koruna
type NGN = [u|NGN|] -- ^ Nigerian naira
type PKR = [u|PKR|] -- ^ Pakistani rupee
type RON = [u|RON|] -- ^ Romanian leu
type DZD = [u|DZD|] -- ^ Algerian dinar
type PEN = [u|PEN|] -- ^ Peruvian neuvo sol
type KZT = [u|KZT|] -- ^ Kazakhstani tenge
type NZD = [u|NZD|] -- ^ New Zealand dollar
type UAH = [u|UAH|] -- ^ Ukrainian hryvnia
type HUF = [u|HUF|] -- ^ Hungarian forint
type QAR = [u|QAR|] -- ^ Qatari riyal
type KWD = [u|KWD|] -- ^ Kuwaiti dinar
type VND = [u|VND|] -- ^ Vietnamese đồng
type BDT = [u|BDT|] -- ^ Bangladeshi taka
type MAD = [u|MAD|] -- ^ Moroccan dirham
type AOA = [u|AOA|] -- ^ Angolan kwanza
type SDG = [u|SDG|] -- ^ Sudanese pound
type LYD = [u|LYD|] -- ^ Libyan dinar
type CUC = [u|CUC|] -- ^ Cuban convertible peso
type CUP = [u|CUP|] -- ^ Cuban peso
type HRK = [u|HRK|] -- ^ Croatian kuna
type SYP = [u|SYP|] -- ^ Syrian pound
type OMR = [u|OMR|] -- ^ Omani rial
type BYR = [u|BYR|] -- ^ Belarusian ruble
type AZN = [u|AZN|] -- ^ Azerbaijani manat
type DOP = [u|DOP|] -- ^ Dominican peso
type LKR = [u|LKR|] -- ^ Sri Lankan rupee
type BGN = [u|BGN|] -- ^ Bulgarian lev
type TND = [u|TND|] -- ^ Tunisian dinar
type MMK = [u|MMK|] -- ^ Burmese kyat
type GTQ = [u|GTQ|] -- ^ Guatemalan quetzal
type UYU = [u|UYU|] -- ^ Uruguayan peso
type LBP = [u|LBP|] -- ^ Lebanese pound
type UZS = [u|UZS|] -- ^ Uzbekistani som
type RSD = [u|RSD|] -- ^ Serbian dinar
type LTL = [u|LTL|] -- ^ Lithuanian litas
type CRC = [u|CRC|] -- ^ Costa Rican colón
type YER = [u|YER|] -- ^ Yemeni rial
type GHS = [u|GHS|] -- ^ Ghana cedi
type KES = [u|KES|] -- ^ Kenyan shilling
type IQD = [u|IQD|] -- ^ Iraqi dinar
type JOD = [u|JOD|] -- ^ Jordanian dinar
type MOP = [u|MOP|] -- ^ Macanese pataca
type ETB = [u|ETB|] -- ^ Ethiopian birr
type PAB = [u|PAB|] -- ^ Panamanian balboa
type LVL = [u|LVL|] -- ^ Latvian lats
type XAF = [u|XAF|] -- ^ Central African CFA franc
type TMT = [u|TMT|] -- ^ Turkmenistan manat
type BHD = [u|BHD|] -- ^ Bahraini dinar
type XOF = [u|XOF|] -- ^ West African CFA franc
type TZS = [u|TZS|] -- ^ Tanzanian shilling
type SVC = [u|SVC|] -- ^ Salvadoran colón
type TTD = [u|TTD|] -- ^ Trinidad and Tobago dollar
type BOB = [u|BOB|] -- ^ Bolivian boliviano

type BTC = [u|BTC|] -- ^ Bitcoin

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

