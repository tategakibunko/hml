-- Copyright (c) 2012 - Watanabe Masaki
-- License: see LICENSE

module Mahjong.Machi (
  isWaitable,
  isRyanmen,
  isTanki,
  isShanpon
) where

import Mahjong.Data

isWaitable :: Pai -> Machi -> Bool
isWaitable p (Ryanmen p1 p2) = p `elem` [p1, p2]
isWaitable p (Shanpon p1 p2) = p `elem` [p1, p2]
isWaitable p (Kanchan p1) = p == p1
isWaitable p (Penchan p1) = p == p1
isWaitable p (Tanki p1) = p == p1
isWaitable p (Tamen ps) = p `elem` ps

isRyanmen :: Machi -> Bool
isRyanmen (Ryanmen _ _) = True
isRyanmen _ = False

isTanki :: Machi -> Bool
isTanki (Tanki _) = True
isTanki _ = False

isShanpon :: Machi -> Bool
isShanpon (Shanpon _ _) = True
isShanpon _ = False



