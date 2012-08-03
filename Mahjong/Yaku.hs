-- Copyright (c) 2012 - Watanabe Masaki
-- License: see LICENSE

module Mahjong.Yaku (
  cutYakuConflict
) where

import qualified Data.List as L
import Mahjong.Data

yakuConflicts :: [(Yaku, [Yaku])]
yakuConflicts =
    [(RYANPEKO,       [CHITOITSU]),
     (ITTSU,          [ITTSU_NAKI]),
     (CHANTA,         [CHANTA_NAKI]),
     (SANSHOKU_DOJUN, [SANSHOKU_DOJUN_NAKI]),
     (SANSHOKU_DOKO,  [SANSHOKU_DOKO_NAKI]),
     (JUNCHANTA,      [CHANTA, CHANTA_NAKI, JUNCHANTA_NAKI]),
     (HONITSU,        [HONITSU_NAKI]),
     (SHOSANGEN,      [SANANKO, HONITSU] ++ [YAKUHAI_HAKU .. YAKUHAI_CHUN]),
     (HONROTO,        [CHANTA, TOITOI]),
     (CHINITSU,       [CHINITSU_NAKI]),
     (TUISO,          [TUMO_AGARI, SANANKO, HONITSU, CHANTA] ++ [YAKUHAI_TON .. YAKUHAI_PEI]),
     (DAISUSHI,       [TUMO_AGARI, SANANKO, HONITSU, CHANTA] ++ [YAKUHAI_TON .. YAKUHAI_PEI]),
     (SHOSUSHI,       [TUMO_AGARI, SANANKO, HONITSU, CHANTA] ++ [YAKUHAI_TON .. YAKUHAI_PEI]),
     (DAISANGEN,      [TUMO_AGARI, SANANKO, HONITSU, CHANTA] ++ [YAKUHAI_TON .. YAKUHAI_PEI]),
     (SUANKO,         [TUMO_AGARI, TOITOI, HONITSU, CHINITSU, HONROTO, SANSHOKU_DOKO])
    ]

cutYakuConflict :: [Yaku] -> [Yaku]
cutYakuConflict ys =
    foldl cuts ys yakuConflicts
    where cuts ys (parent, confs) =
              if parent `elem` ys then
                  foldl (\rest y -> L.delete y rest) ys confs
              else ys

