-- Copyright (c) 2012 - Watanabe Masaki
-- License: see LICENSE

module Mahjong.Parser(
  parseMachi,
  parseAgari
) where

import qualified Data.List as L
import Ix (inRange)
import Data.Maybe (fromJust)
import Control.Monad (guard)
import Mahjong.Data
import Mahjong.Pai
import Mahjong.Mentsu
import Mahjong.Machi
import Mahjong.Yaku

cutPais :: [Pai] -> [Pai] -> [Pai]
cutPais srcps delps = foldl (\ps dp -> L.delete dp ps) srcps delps

cutMentsu :: [Pai] -> Mentsu -> [Pai]
cutMentsu pais m = cutPais pais $ paisOfMentsu m

findSeq :: Pai -> Int -> [Pai] -> Bool
findSeq start count pais =
    all ((flip elem) pais) $ take count [start ..]

findSame :: Pai -> Int -> [Pai] -> Bool
findSame start count pais = 
    (length $ filter (start ==) pais) >= count

fetchShun :: Pai -> [Pai] -> Maybe Mentsu
fetchShun start pais
    | isJihai start = Nothing
    | pnumOfPai start > 7 = Nothing
    | findSeq start 3 pais = Just $ Shuntsu start
    | otherwise = Nothing

fetchKou :: Pai -> [Pai] -> Maybe Mentsu
fetchKou start pais =
    if findSame start 3 pais then
        Just $ Koutsu start
    else Nothing

countPai :: Pai -> [Pai] -> Int
countPai p1 ps =
    length $ filter (p1 ==) ps

countMentsu :: Mentsu -> [Mentsu] -> Int
countMentsu m1 ms =
    length $ filter (m1 ==) ms

countToitsu :: [Pai] -> Int
countToitsu pais =
    let ps = L.nub pais in
    length $ filter (\p -> countPai p pais == 2) ps

countIpeko :: [Mentsu] -> Int
countIpeko mentsu =
    let mshun = filter isShun mentsu
        ms = L.nub mshun in
    foldl (\c m -> if countMentsu m mshun > 1 then c + 1 else c) 0 ms

countAnko :: [Mentsu] -> Int
countAnko mentsu =
    length $ filter isKou mentsu

parseA1 :: [Pai] -> [(Mentsu, [Pai])]
parseA1 pais = do
  p <- L.nub pais
  guard $ (length $ filter (p==) pais) >= 2
  return $ (Atama p, cutMentsu pais $ Atama p)

parseM1 :: [Pai] -> [(Mentsu, [Pai])]
parseM1 pais = do
  start <- L.nub pais
  m <- [fetchShun start pais, fetchKou start pais]
  guard (m /= Nothing)
  return $ (fromJust m, cutMentsu pais $ fromJust m)

parseA1Mn :: [Pai] -> [[Mentsu]]
parseA1Mn pais = do
  (a1, rest) <- parseA1 pais
  parseA1MnRest [a1] rest

parseA1MnRest :: [Mentsu] -> [Pai] -> [[Mentsu]]
parseA1MnRest ms [] = [L.sort ms]
parseA1MnRest ms pais = do
  (m, rest) <- parseM1 pais
  parseA1MnRest (m : ms) rest

parseMachiP2 :: Mentsu -> [Pai] -> Maybe Machi
parseMachiP2 atama ps2
    | p1 == p2 =
        case atama of
          Atama p ->
              if p1 < p then
                  Just $ Shanpon p1 p
              else Just $ Shanpon p p1
          otherwise -> Nothing
    | inRange(1, 8) pn1 && succ p1 == p2 =
        case pn1 of
          1 -> Just $ Penchan $ succ p2
          8 -> Just $ Penchan $ pred p1
          otherwise -> Just $ Ryanmen (pred p1) (succ p2)
    | inRange(1, 7) pn1 && pn1 < 8 && succ p1 == pred p2 =
        Just $ Kanchan (succ p1)
    | otherwise = Nothing
    where p1 = ps2 !! 0
          p2 = ps2 !! 1
          pn1 = pnumOfPai p1
          pn2 = pnumOfPai p2
                        
parseMachiA1Mn :: [Pai] -> [Machi]
parseMachiA1Mn [] = []
parseMachiA1Mn pais = do
  (a1, rest) <- parseA1 pais
  parseMachiA1MnRest a1 $ L.sort rest

parseMachiA1MnRest :: Mentsu -> [Pai] -> [Machi]
parseMachiA1MnRest a1 pais 
    | plen > 2 =
        do
          (m, rest) <- parseM1 pais
          parseMachiA1MnRest a1 rest

    | plen == 2 =
        case parseMachiP2 a1 pais of
          Just m -> [m]
          Nothing -> []

    | plen == 1 = [Tanki $ head pais]
    | otherwise = []
    where plen = length pais

parseMachiMn :: [Pai] -> [Machi]
parseMachiMn [] = []
parseMachiMn [p] = [Tanki p]
parseMachiMn pais = do
  (m, rest) <- parseM1 pais
  parseMachiMn $ L.sort rest

parseMachiNormal :: [Pai] -> [Machi]
parseMachiNormal pais =
    (parseMachiA1Mn pais) ++ (parseMachiMn pais)

parseMachiChitoitsu :: [Pai] -> [Machi]
parseMachiChitoitsu pais =
    if countToitsu pais == 6 then
        let ps = L.nub pais in
        case L.find (\p -> (length $ filter (p==) pais) == 1) ps of
          Just p -> [Tanki p]
          Nothing -> []
    else []

parseMachiKokushi :: [Pai] -> [Machi]
parseMachiKokushi pais =
    let ps = L.nub pais in
    if length ps >= 12 && all isYaochuhai ps then
        case L.find (\r -> r `notElem` ps) yaochuhai of
          Just p -> [Tanki p]
          Nothing -> [Tamen yaochuhai]
    else []

parseMachi :: [Pai] -> [Machi]
parseMachi pais =
    L.nub $ (parseMachiNormal pais) ++ (parseMachiChitoitsu pais) ++ (parseMachiKokushi pais)

-- 1 han
isTumoAgari :: NakiMentsu -> FetchType -> Bool
isTumoAgari naki fetch =
    getNakiMentsu naki == [] && fetch == TUMO

isTanyao :: [Mentsu] -> Bool
isTanyao mentsu =
    all (not . isChantaMentsu) mentsu

isPinfu :: Machi -> [Mentsu] -> Bool
isPinfu machi mentsu =
    isRyanmen machi && (length $ filter isShun mentsu) == 4

isIpeko :: [Mentsu] -> Bool
isIpeko mentsu =
    countIpeko mentsu == 1

isYakuhai :: Pai -> [Mentsu] -> Bool
isYakuhai pai mentsu =
    isJihai pai && (Koutsu pai) `elem` mentsu

-- 2 han
isChanta :: [Mentsu] -> Bool
isChanta mentsu =
    all isChantaMentsu mentsu

isSanshokuDojun :: [Mentsu] -> Bool
isSanshokuDojun mentsu =
    any (\g -> length g >= 3) $ L.groupBy (==) $ map pnumOfPai $ L.nub $ map paiOfMentsu $ filter isShun mentsu

isSanshokuDoko :: [Mentsu] -> Bool
isSanshokuDoko mentsu =
    any (\g -> length g >= 3) $ L.groupBy (==) $ map pnumOfPai $ L.nub $ filter isSuhai $ map paiOfMentsu $ filter isKou mentsu

isSananko :: FetchType -> Machi -> [Mentsu] -> Bool
isSananko fetch machi mentsu
    | countAnko mentsu /= 3 = False
    | fetch == TUMO = True
    | fetch == RON && isShanpon machi == False = True
    | otherwise = False

isChitoitsu :: Machi -> [Pai] -> Bool
isChitoitsu machi pais =
    isTanki machi && countToitsu pais == 7

isIttsu :: [Mentsu] -> Bool
isIttsu mentsu =
    let sps = map paiOfMentsu $ filter isShun mentsu
        pat = [[M1, M4, M7],
               [P1, P4, P7],
               [S1, S4, S7]] in
    any (\pt -> all (\m-> m `elem` sps) pt) pat

isToitoi :: [Mentsu] -> Bool
isToitoi mentsu =
    countAnko mentsu == 4

-- 3 han
isRyanpeko :: [Mentsu] -> Bool
isRyanpeko mentsu =
    countIpeko mentsu == 2

isJunchanta :: [Mentsu] -> Bool
isJunchanta mentsu =
    all isJunchanMentsu mentsu

isHonitsu :: [Pai] -> Bool
isHonitsu pais =
    any isJihai pais && (length $ L.nub $ map familyOfPai $ filter (not . isJihai) pais) == 1

-- 4 han
isShosangen :: [Mentsu] -> Bool
isShosangen mentsu =
    sang_a_count == 1 && sang_k_count == 2
    where ks = filter isKou mentsu
          as = filter isAtama mentsu
          sang_k_count = length $ filter (isSang . paiOfMentsu) ks
          sang_a_count = length $ filter (isSang . paiOfMentsu) as

isHonroto :: [Pai] -> Bool
isHonroto pais =
    all isYaochuhai pais

-- 6 han
isChinitsu :: [Pai] -> Bool
isChinitsu pais =
    (length $ L.nub $ map familyOfPai pais) == 1

-- yakuman
isKokushi :: [Pai] -> Bool
isKokushi pais =
    let ps = L.nub pais in
    length ps == 13 && all isYaochuhai ps

isSuanko :: FetchType -> Machi -> [Mentsu] -> Bool
isSuanko fetch machi mentsu
    | countAnko mentsu /= 4 = False
    | fetch == TUMO = True
    | fetch == RON && isShanpon machi == False = True
    | otherwise = False

isSuanko2 :: Machi -> [Mentsu] -> Bool
isSuanko2 machi mentsu = isTanki machi && countAnko mentsu == 4

isDaisangen :: [Mentsu] -> Bool
isDaisangen mentsu =
    all (\k -> k `elem` mentsu) [Koutsu HAKU, Koutsu HATSU, Koutsu CHUN]

isDaisushi :: [Mentsu] -> Bool
isDaisushi mentsu =
    (length $ filter (isKaze . paiOfMentsu) $ filter isKou mentsu) == 4

isShosushi :: [Mentsu] -> Bool
isShosushi mentsu =
    kaze_kcount == 3 && kaze_acount == 1
    where kaze_kcount = length $ filter (isKaze . paiOfMentsu) $ filter isKou mentsu
          kaze_acount = length $ filter (isKaze . paiOfMentsu) $ filter isAtama mentsu

isChinroto :: [Mentsu] -> Bool
isChinroto mentsu =
    all (isRotohai . paiOfMentsu) mentsu

isTuiso :: [Mentsu] -> Bool
isTuiso mentsu =
    all (isJihai . paiOfMentsu) mentsu

isRyuiso :: [Mentsu] -> Bool
isRyuiso mentsu =
    all (isGreenhai . paiOfMentsu) mentsu

isChurenpoto :: [Pai] -> Bool
isChurenpoto pais =
    countPai M1 pais >= 3 &&
    countPai M9 pais >= 3 &&
    all (\p -> p `elem` pais) [M2 .. M8]

parseAgariNormal :: NakiMentsu -> FetchType -> Pai -> [Pai] -> [Result]
parseAgariNormal naki fetch p1 pais = do
    machi <- filter (isWaitable p1) $ L.nub $ parseMachiNormal pais
    mentsu <- L.nub $ parseA1Mn psall
    let yaku = cutYakuConflict $ filter (checkYaku machi mentsu) [(minBound :: Yaku) .. (maxBound :: Yaku)]
    guard (yaku /= [])
    return $ Agari machi yaku
    where psall = L.sort $ p1 : pais
          checkYaku machi mentsu yaku =
              let naki_mentsu = getNakiMentsu naki
                  mentsu2 = mentsu ++ naki_mentsu
                  psall2 = L.sort $ psall ++ (concat $ map paisOfMentsu naki_mentsu) in
              case yaku of
                TUMO_AGARI -> isTumoAgari naki fetch
                PINFU -> isPinfu machi mentsu
                TANYAO -> isTanyao mentsu
                IPEKO -> isIpeko mentsu
                YAKUHAI_HAKU -> isYakuhai HAKU mentsu2
                YAKUHAI_HATSU -> isYakuhai HATSU mentsu2
                YAKUHAI_CHUN -> isYakuhai CHUN mentsu2

                ITTSU -> naki_mentsu == [] && isIttsu mentsu
                ITTSU_NAKI -> naki_mentsu /= [] && isIttsu mentsu2
                CHANTA -> naki_mentsu == [] && isChanta mentsu
                CHANTA_NAKI -> naki_mentsu /= [] && isChanta mentsu2
                SANSHOKU_DOJUN -> isSanshokuDojun mentsu
                SANSHOKU_DOJUN_NAKI -> naki_mentsu /= [] && isSanshokuDojun mentsu2
                SANSHOKU_DOKO -> isSanshokuDoko mentsu
                SANSHOKU_DOKO_NAKI -> naki_mentsu /= [] && isSanshokuDoko mentsu2
                SANANKO -> isSananko fetch machi mentsu
                TOITOI -> naki_mentsu /= [] && isToitoi mentsu2

                RYANPEKO -> isRyanpeko mentsu
                JUNCHANTA -> naki_mentsu == [] && isJunchanta mentsu
                JUNCHANTA_NAKI -> naki_mentsu /= [] && isJunchanta mentsu2
                HONITSU -> naki_mentsu == [] && isHonitsu psall
                HONITSU_NAKI -> naki_mentsu /= [] && isHonitsu psall2

                SHOSANGEN -> isShosangen mentsu2
                HONROTO -> naki_mentsu /= [] && isHonroto psall2

                CHINITSU -> naki_mentsu == [] && isChinitsu psall
                CHINITSU_NAKI -> naki_mentsu /= [] && isChinitsu psall2

                SUANKO -> naki_mentsu == [] && isSuanko fetch machi mentsu
                SUANKO2 -> naki_mentsu == [] && isSuanko2 machi mentsu
                DAISANGEN -> isDaisangen mentsu
                DAISUSHI -> isDaisushi mentsu
                SHOSUSHI -> isShosushi mentsu
                TUISO -> isTuiso mentsu
                CHINROTO -> isChinroto mentsu
                RYUISO -> isRyuiso mentsu
                CHURENPOTO -> isChurenpoto psall
                otherwise -> False

parseAgariChitoitsu :: NakiMentsu -> FetchType -> Pai -> [Pai] -> [Result]
parseAgariChitoitsu naki fetch p1 pais = do
  machi <- filter (isWaitable p1) $ L.nub $ parseMachiChitoitsu pais
  let yaku = cutYakuConflict $ filter (checkYaku machi) [TUMO_AGARI, CHITOITSU, HONITSU, CHINITSU]
  guard (yaku /= [])
  return $ Agari machi yaku
  where psall = L.sort $ p1 : pais
        checkYaku machi yaku =
            case yaku of
              TUMO_AGARI -> isTumoAgari naki fetch
              CHITOITSU -> isChitoitsu machi psall
              HONITSU -> isHonitsu psall
              CHINITSU -> isChinitsu psall
              otherwise -> False

parseAgariKokushi :: Pai -> [Pai] -> [Result]
parseAgariKokushi p1 pais = do
  machi <- filter (isWaitable p1) $ L.nub $ parseMachiKokushi pais
  guard (isKokushi psall)
  return $ Agari machi [KOKUSHI]
  where psall = L.sort $ p1 : pais

parseAgari :: NakiMentsu -> FetchType -> Pai -> [Pai] -> [Result]
parseAgari naki fetch p1 pais =
    let normal = parseAgariNormal naki fetch p1 pais
        chitoitsu = parseAgariChitoitsu naki fetch p1 pais
        kokushi = parseAgariKokushi p1 pais in
    L.nub $ normal ++ chitoitsu ++ kokushi

