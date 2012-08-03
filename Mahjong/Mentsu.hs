module Mahjong.Mentsu (
  paiOfMentsu,
  paisOfMentsu,
  isShun,
  isKou,
  isAtama,
  isJunchanMentsu,
  isChantaMentsu
) where

import Mahjong.Data
import Mahjong.Pai

paiOfMentsu :: Mentsu -> Pai
paiOfMentsu (Shuntsu p) = p
paiOfMentsu (Koutsu p) = p
paiOfMentsu (Kantsu p) = p
paiOfMentsu (Atama p) = p

paisOfMentsu :: Mentsu -> [Pai]
paisOfMentsu (Shuntsu p) = take 3 [p ..]
paisOfMentsu (Koutsu p) = replicate 3 p
paisOfMentsu (Kantsu p) = replicate 4 p
paisOfMentsu (Atama p) = replicate 2 p

isShun :: Mentsu -> Bool
isShun (Shuntsu _) = True
isShun _ = False

isKou :: Mentsu -> Bool
isKou (Koutsu _) = True
isKou _ = False

isAtama :: Mentsu -> Bool
isAtama (Atama _) = True
isAtama _ = False

isJunchanMentsu :: Mentsu -> Bool
isJunchanMentsu (Shuntsu p) = pnumOfPai p `elem` [1, 7]
isJunchanMentsu (Koutsu p) = isRotohai p
isJunchanMentsu (Kantsu p) = isRotohai p
isJunchanMentsu (Atama p) = isRotohai p

isChantaMentsu :: Mentsu -> Bool
isChantaMentsu (Shuntsu p) = isJunchanMentsu (Shuntsu p)
isChantaMentsu (Koutsu p) = isYaochuhai p
isChantaMentsu (Kantsu p) = isYaochuhai p
isChantaMentsu (Atama p) = isYaochuhai p

