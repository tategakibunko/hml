module Main where

import Test.HUnit
import Mahjong.Data
import Mahjong.Parser
import qualified Data.List as L
import qualified Mahjong.Pai as P

machiOfAgari :: Result -> Machi
machiOfAgari (Agari machi _) = machi

yakuOfAgari :: Result -> [Yaku]
yakuOfAgari (Agari _ ys) = ys

checkYaku :: [Result] -> Yaku -> Bool
checkYaku ret yaku =
    any (\agari -> yaku `elem` (yakuOfAgari agari)) ret

checkYakuMany :: [Result] -> [Yaku] -> Bool
checkYakuMany ret ys =
    any (\agari -> all (\y -> y `elem` ys) $ yakuOfAgari agari) ret

testTumoAgari yaku =
    let ret = parseAgari (NakiMentsu []) TUMO P7 [P1,P1,P2,P2,P3,P3,P4,P4,P5,P5,P6,P6,P7] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testTanyao yaku =
    let ret = parseAgari (NakiMentsu []) TUMO P7 [M2,M2,P2,P2,P3,P3,P4,P4,P5,P5,P6,P6,P7] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testYakuhaiHaku yaku =
    let ret = parseAgari (NakiMentsu [Koutsu HAKU]) TUMO P9 [M2,M2,P1,P2,P3,P4,P5,P6,P7,P8] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testYakuhaiHatsu yaku =
    let ret = parseAgari (NakiMentsu [Koutsu HATSU]) TUMO P9 [M2,M2,P1,P2,P3,P4,P5,P6,P7,P8] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testYakuhaiChun yaku =
    let ret = parseAgari (NakiMentsu [Koutsu CHUN]) TUMO P9 [M2,M2,P1,P2,P3,P4,P5,P6,P7,P8] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testPinfu yaku =
    let ret = parseAgari (NakiMentsu []) TUMO P7 [P1,P1,P2,P2,P3,P3,P4,P4,P5,P5,P6,P6,P7] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testIpeko yaku =
    let ret = parseAgari (NakiMentsu []) TUMO P8 [M2,M2,S2,S3,S4,P3,P4,P5,P6,P6,P7,P7,P8] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testSanshokuDojun yaku =
    let ret = parseAgari (NakiMentsu []) TUMO TON [M2,M2,M4,M5,M6,P4,P5,P6,S4,S5,S6,TON,TON] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testSanshokuDojunNaki yaku =
    let ret = parseAgari (NakiMentsu [Shuntsu M4]) TUMO TON [M2,M2,P4,P5,P6,S4,S5,S6,TON,TON] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testSanshokuDoko yaku =
    let ret = parseAgari (NakiMentsu []) TUMO NAN [M2,M2,M2,P2,P2,P2,S2,S2,S2,P5,P6,P7,NAN] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testSanshokuDokoNaki yaku =
    let ret = parseAgari (NakiMentsu [Koutsu M2]) TUMO NAN [P2,P2,P2,S2,S2,S2,P5,P6,P7,NAN] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testIttsu yaku =
    let ret = parseAgari (NakiMentsu []) TUMO NAN [M2,M2,M2,P1,P2,P3,P4,P5,P6,P7,P8,P9,NAN] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testIttsuNaki yaku =
    let ret = parseAgari (NakiMentsu [Shuntsu P1]) TUMO NAN [M2,M2,M2,P4,P5,P6,P7,P8,P9,NAN] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testToitoi yaku =
    let ret = parseAgari (NakiMentsu [Koutsu TON]) TUMO NAN [P1,P1,P1,P4,P4,P4,P6,P6,NAN,NAN] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testChitoitsu yaku =
    let ret = parseAgari (NakiMentsu []) TUMO NAN [P1,P1,P2,P2,P4,P4,P6,P6,TON,TON,S1,S1,NAN] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testChanta yaku =
    let ret = parseAgari (NakiMentsu []) TUMO NAN [M1,M2,M3,P1,P2,P3,S1,S2,S3,P9,P9,P9,NAN] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testChantaNaki yaku =
    let ret = parseAgari (NakiMentsu [Koutsu P9]) TUMO NAN [M1,M2,M3,P1,P2,P3,S1,S2,S3,NAN] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testJunchanta yaku =
    let ret = parseAgari (NakiMentsu []) TUMO S9 [M1,M2,M3,P1,P2,P3,S1,S2,S3,P9,P9,P9,S9] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testJunchantaNaki yaku =
    let ret = parseAgari (NakiMentsu [Koutsu P9]) TUMO S9 [M1,M2,M3,P1,P2,P3,S1,S2,S3,S9] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testRyanpeko yaku =
    let ret = parseAgari (NakiMentsu []) TUMO P7 [P1,P1,P2,P2,P3,P3,P4,P4,P5,P5,P6,P6,P7] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testHonitsu yaku =
    let ret = parseAgari (NakiMentsu []) TUMO NAN [M2,M2,M2,M1,M2,M3,M4,M5,M6,TON,TON,TON,NAN] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testHonitsuNaki yaku =
    let ret = parseAgari (NakiMentsu [Koutsu TON]) TUMO NAN [M2,M2,M2,M1,M2,M3,M4,M5,M6,NAN] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testHonroto yaku =
    let ret = parseAgari (NakiMentsu [Koutsu TON]) TUMO NAN [M1,M1,M1,M9,M9,M9,P1,P1,P1,NAN] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testChinitsu yaku =
    let ret = parseAgari (NakiMentsu []) TUMO M7 [M1,M1,M1,M9,M9,M9,M2,M3,M4,M5,M6,M7,M7] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testChinitsuNaki yaku =
    let ret = parseAgari (NakiMentsu [Shuntsu M2]) TUMO M7 [M1,M1,M1,M9,M9,M9,M5,M6,M7,M7] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testSuanko yaku =
    let ret = parseAgari (NakiMentsu []) TUMO M1 [P1,P1,P1,P2,P2,P2,P3,P3,P3,S8,S8,M1,M1] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testSuanko2 yaku =
    let ret = parseAgari (NakiMentsu []) TUMO M1 [P1,P1,P1,P2,P2,P2,P3,P3,P3,S8,S8,S8,M1] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testDaisangen yaku =
    let ret = parseAgari (NakiMentsu []) TUMO M1 [HAKU,HAKU,HAKU,HATSU,HATSU,HATSU,CHUN,CHUN,CHUN,P1,P2,P3,M1] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testKokushi yaku =
    let ret = parseAgari (NakiMentsu []) TUMO NAN [M1,M9,P1,P9,S1,S9,HAKU,HATSU,CHUN,TON,SHA,PEI,PEI] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testRyuiso yaku =
    let ret = parseAgari (NakiMentsu []) TUMO HATSU [S2,S2,S3,S3,S4,S4,S6,S6,S6,S8,S8,S8,HATSU] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testShosushi yaku =
    let ret = parseAgari (NakiMentsu []) TUMO M1 [TON,TON,TON,NAN,NAN,NAN,SHA,SHA,SHA,PEI,PEI,M2,M3] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testDaisushi yaku =
    let ret = parseAgari (NakiMentsu []) TUMO M1 [TON,TON,TON,NAN,NAN,NAN,SHA,SHA,SHA,PEI,PEI,PEI,M1] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testTuiso yaku =
    let ret = parseAgari (NakiMentsu []) TUMO HATSU [TON,TON,TON, NAN,NAN,NAN, CHUN,CHUN,CHUN, HAKU,HAKU, HATSU,HATSU] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testChinroto yaku =
    let ret = parseAgari (NakiMentsu []) TUMO M1 [P1,P1,P1,P9,P9,P9,S1,S1,S1,S9,S9,M1,M1] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

testChurenpoto yaku =
    let ret = parseAgari (NakiMentsu []) TUMO M1 [M1,M1,M1, M2,M3,M4, M5,M6,M7, M8, M9,M9,M9] in
    TestCase (assertEqual (show ret) True $ checkYaku ret yaku)

labelCase fn yaku =
    TestLabel (show yaku) (fn yaku)

yakuTests =
    TestList [labelCase testTumoAgari TUMO_AGARI,
              labelCase testTanyao TANYAO,
              labelCase testIpeko IPEKO,
              labelCase testPinfu PINFU,
              labelCase testYakuhaiHaku YAKUHAI_HAKU,
              labelCase testYakuhaiHatsu YAKUHAI_HATSU,
              labelCase testYakuhaiChun YAKUHAI_CHUN,

              labelCase testIttsu ITTSU,
              labelCase testIttsuNaki ITTSU_NAKI,
              labelCase testChanta CHANTA,
              labelCase testChantaNaki CHANTA_NAKI,
              labelCase testSanshokuDojun SANSHOKU_DOJUN,
              labelCase testSanshokuDojunNaki SANSHOKU_DOJUN_NAKI,
              labelCase testSanshokuDoko SANSHOKU_DOKO,
              labelCase testSanshokuDokoNaki SANSHOKU_DOKO_NAKI,
              labelCase testChitoitsu CHITOITSU,

              labelCase testRyanpeko RYANPEKO,
              labelCase testJunchanta JUNCHANTA,
              labelCase testJunchantaNaki JUNCHANTA_NAKI,
              labelCase testHonitsu HONITSU,
              labelCase testHonitsuNaki HONITSU_NAKI,

              labelCase testHonroto HONROTO,

              labelCase testChinitsu CHINITSU,
              labelCase testChinitsuNaki CHINITSU_NAKI,

              labelCase testSuanko SUANKO,
              labelCase testSuanko2 SUANKO2,
              labelCase testDaisangen DAISANGEN,
              labelCase testRyuiso RYUISO,
              labelCase testShosushi SHOSUSHI,
              labelCase testDaisushi DAISUSHI,
              labelCase testTuiso TUISO,
              labelCase testChinroto CHINROTO,
              labelCase testChurenpoto CHURENPOTO,
              labelCase testKokushi KOKUSHI
             ]

testRyanmen =
    let ret = parseMachi [S1,S1,P2,P3] in
    TestCase (assertEqual (show ret) True $ (Ryanmen P1 P4) `elem` ret)

testPenchan =
    let ret = parseMachi [S1,S1,P1,P2] in
    TestCase (assertEqual (show ret) True $ (Penchan P3) `elem` ret)

testKanchan =
    let ret = parseMachi [S1,S1,P1,P3] in
    TestCase (assertEqual (show ret) True $ (Kanchan P2) `elem` ret)

testShanpon =
    let ret = parseMachi [S1,S1,P1,P1] in
    TestCase (assertEqual (show ret) True $ (Shanpon S1 P1) `elem` ret)

testTanki =
    let ret = parseMachi [S1,S1,P1] in
    TestCase (assertEqual (show ret) True $ (Tanki P1) `elem` ret)

testTamen =
    let ret = parseMachi [M1,M9,P1,P9,S1,S9,TON,NAN,SHA,PEI,HAKU,HATSU,CHUN] in
    TestCase (assertEqual (show ret) True $ (Tamen P.yaochuhai) `elem` ret)

machiTests =
    TestList [TestLabel "ryanmen" testRyanmen,
              TestLabel "penchan" testPenchan,
              TestLabel "kanchan" testKanchan,
              TestLabel "shanpon" testShanpon,
              TestLabel "tanki"   testTanki,
              TestLabel "tamen1"  testTamen
             ]

debugHaipai :: Pai -> [Pai] -> IO ()
debugHaipai p1 pais = do
  putStrLn $ (show $ L.sort pais) ++ " + " ++ (show p1)

debugAgari :: FetchType -> Pai -> [Pai] -> IO ()
debugAgari fetch p1 pais = do
  debugHaipai p1 pais
  putStrLn $ unlines $ map show $ parseAgari (NakiMentsu []) fetch p1 pais

debug = do
  debugAgari RON P7 [P1,P1,P2,P2,P3,P3,P4,P4,P5,P5,P6,P6,P7] 
  debugAgari RON P9 [P1,P2,P3,P4,P5,P6,P7,P8,M1,M1,S9,S9,S9]
  debugAgari RON P8 [M1,M2,M3,P1,P2,P3,S1,S2,S3,P5,P5,P6,P7]
  debugAgari RON P8 [M2,M3,M4,P2,P3,P4,S2,S3,S4,P5,P5,P6,P7]
  debugAgari RON P9 [M1,M2,M3,P1,P2,P3,S1,S2,S3,P7,P8,P1,P1]
  debugAgari RON CHUN [M1,M9,P1,P9,S1,S9,TON,NAN,SHA,PEI,HAKU,HAKU,HATSU]
  debugAgari RON CHUN [HAKU,HAKU,HAKU,HATSU,HATSU,HATSU,CHUN,CHUN,P1,P2,P3,P4,P4]
  debugAgari RON CHUN [P1,P1,P2,P2,P3,P3,P4,P4,P5,P5,P6,P6,CHUN]
  debugAgari RON P4 [HAKU,HAKU,HAKU,HATSU,HATSU,HATSU,CHUN,CHUN,P1,P2,P3,P4,P4]
  debugAgari TUMO P1 [TON,TON,TON,NAN,NAN,NAN,M1,M1,P9,P9,P9,P1,P1]

main = do
  runTestTT yakuTests
  runTestTT machiTests



