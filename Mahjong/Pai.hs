module Mahjong.Pai (
 rotohai,
 yaochuhai,
 greenhai,
 familyOfPai,
 pnumOfPai,
 paiOfPcode,
 pcodeOfPai,
 encodePais,
 decodePais,
 isManz,
 isPinz,
 isSouz,
 isKaze,
 isSang,
 isSuhai,
 isJihai,
 isRotohai,
 isYaochuhai,
 isGreenhai
) where

import Ix (inRange)
import Mahjong.Data

rotohai :: [Pai]
rotohai = [M1,M9,P1,P9,S1,S9]

yaochuhai :: [Pai]
yaochuhai = rotohai ++ [TON .. CHUN]

greenhai = [S2,S3,S4,S6,S8,HATSU]

familyOfPai :: Pai -> Family
familyOfPai pai
    | pai `elem` [M1 .. M9] = MANZ
    | pai `elem` [P1 .. P9] = PINZ
    | pai `elem` [S1 .. S9] = SOUZ
    | pai `elem` [TON .. PEI] = KAZE
    | pai `elem` [HAKU .. CHUN] = SANG

paiOfPcode :: Int -> Pai
paiOfPcode = toEnum

pcodeOfPai :: Pai -> Int
pcodeOfPai = fromEnum

pnumOfPai :: Pai -> Int
pnumOfPai pai
    | pai `elem` [M1 .. M9] = pcode - pcodeOfPai M1 + 1
    | pai `elem` [P1 .. P9] = pcode - pcodeOfPai P1 + 1
    | pai `elem` [S1 .. S9] = pcode - pcodeOfPai S1 + 1
    | otherwise = 0
    where pcode = pcodeOfPai pai
        
encodePais :: [Pai] -> [Int]
encodePais = map pcodeOfPai 

decodePais :: [Int] -> [Pai]
decodePais = map paiOfPcode

isManz :: Pai -> Bool
isManz pai = inRange (pcodeOfPai M1, pcodeOfPai M9) $ pcodeOfPai pai

isPinz :: Pai -> Bool
isPinz pai = inRange (pcodeOfPai P1, pcodeOfPai P9) $ pcodeOfPai pai

isSouz :: Pai -> Bool
isSouz pai = inRange (pcodeOfPai S1, pcodeOfPai S9) $ pcodeOfPai pai

isKaze :: Pai -> Bool
isKaze pai = inRange (pcodeOfPai TON, pcodeOfPai PEI) $ pcodeOfPai pai

isSang :: Pai -> Bool
isSang pai = inRange (pcodeOfPai HAKU, pcodeOfPai CHUN) $ pcodeOfPai pai

isSuhai :: Pai -> Bool
isSuhai pai = pcodeOfPai pai < pcodeOfPai TON

isJihai :: Pai -> Bool
isJihai = not . isSuhai

isRotohai :: Pai -> Bool
isRotohai pai = pai `elem` rotohai

isYaochuhai :: Pai -> Bool
isYaochuhai pai = pai `elem` yaochuhai

isGreenhai :: Pai -> Bool
isGreenhai pai = pai `elem` greenhai

