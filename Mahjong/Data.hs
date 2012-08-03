-- Copyright (c) 2012 - Watanabe Masaki
-- License: see LICENSE

module Mahjong.Data (
  Pai(..),
  Family(..),
  Mentsu(..),
  Machi(..),
  Yaku(..),
  FetchType(..),
  NakiMentsu(..),
  Result(..)
) where

import qualified Data.List as L

data Pai = M1 | M2 | M3 | M4 | M5 | M6 | M7 | M8 | M9
         | P1 | P2 | P3 | P4 | P5 | P6 | P7 | P8 | P9
         | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9
         | TON | NAN | SHA | PEI
         | HAKU | HATSU | CHUN deriving (Show, Ord, Enum, Eq)

data FetchType = TUMO
               | RON deriving (Show, Eq)

data Family = MANZ
            | PINZ
            | SOUZ
            | KAZE
            | SANG deriving (Show, Ord, Eq, Enum)

data Mentsu = Shuntsu Pai
            | Koutsu Pai
            | Kantsu Pai
            | Atama Pai deriving (Ord)

data Machi = Ryanmen Pai Pai
           | Shanpon Pai Pai
           | Kanchan Pai
           | Penchan Pai
           | Tanki Pai
           | Tamen [Pai] deriving (Show)

data Yaku = TUMO_AGARI
          | TANYAO
          | PINFU
          | IPEKO
          | YAKUHAI_TON
          | YAKUHAI_NAN
          | YAKUHAI_SHA
          | YAKUHAI_PEI
          | YAKUHAI_HAKU
          | YAKUHAI_HATSU
          | YAKUHAI_CHUN

          | ITTSU
          | ITTSU_NAKI
          | CHANTA
          | CHANTA_NAKI
          | SANANKO
          | SANSHOKU_DOJUN
          | SANSHOKU_DOJUN_NAKI
          | SANSHOKU_DOKO
          | SANSHOKU_DOKO_NAKI
          | TOITOI
          | CHITOITSU

          | JUNCHANTA
          | JUNCHANTA_NAKI
          | RYANPEKO
          | HONITSU
          | HONITSU_NAKI

          | SHOSANGEN
          | HONROTO

          | CHINITSU
          | CHINITSU_NAKI

          | SUANKO
          | SUANKO2
          | DAISANGEN
          | DAISUSHI
          | SHOSUSHI
          | CHINROTO
          | TUISO
          | RYUISO
          | CHURENPOTO
          | KOKUSHI deriving (Eq, Show, Enum, Bounded)

data Result = Agari Machi [Yaku] deriving (Eq, Show)

newtype NakiMentsu = NakiMentsu { getNakiMentsu :: [Mentsu] }

instance Show Mentsu where
    show (Shuntsu p) = show $ take 3 [p ..]
    show (Koutsu p) = show $ replicate 3 p
    show (Kantsu p) = show $ replicate 4 p
    show (Atama p) = show $ replicate 2 p

instance Eq Mentsu where
    (==) (Shuntsu p1) (Shuntsu p2) = p1 == p2
    (==) (Koutsu p1) (Koutsu p2) = p1 == p2
    (==) (Kantsu p1) (Kantsu p2) = p1 == p2
    (==) (Atama p1) (Atama p2) = p1 == p2
    (==) _ _ = False

instance Eq Machi where
    (==) (Ryanmen p1 p2) (Ryanmen p3 p4) = L.sort [p1,p2] == L.sort [p3,p4]
    (==) (Shanpon p1 p2) (Shanpon p3 p4) = L.sort [p1,p2] == L.sort [p3,p4]
    (==) (Penchan p1) (Penchan p2) = p1 == p2
    (==) (Kanchan p1) (Kanchan p2) = p1 == p2
    (==) (Tanki p1) (Tanki p2) = p1 == p2
    (==) (Tamen ps1) (Tamen ps2) = L.sort ps1 == L.sort ps2
    (==) _ _ = False


