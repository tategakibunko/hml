hml
===

haskell mahjong library

## how to use

import Mahjong.Parser

import Mahjong.Data

:t parseAgari
> parseAgari :: NakiMentsu -> FetchType -> Pai -> [Pai] -> [Result]

parseAgari (NakiMentsu []) TUMO P1 [P1,P2,P2,P3,P3,M1,M2,M3,S1,S2,S3,S8,S8]
> [Agari (Ryanmen P1 P4) [TUMO_AGARI,PINFU,IPEKO,SANSHOKU_DOJUN]]

parseAgari (NakiMentsu [Shuntsu P1]) RON P9 [P4,P5,P6,P7,P8,S1,S1,M1,M2,M3]
> [Agari (Ryanmen P6 P9) [ITTSU_NAKI]]`

parseAgari (NakiMentsu [Koutsu P1]) RON TON [P2,P3,P4,P5,P6,P7,NAN,NAN,NAN,TON]
> [Agari (Tanki TON) [HONITSU_NAKI]]
