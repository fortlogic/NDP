module Resources.ROM.Tiles.Map where

import Resources.ROM.Tiles

cbm :: TileMap
cbm = TileMap {
  defaultTile = Empty,
  tiles = [ Tile { name = "A", code = 0x01, content = File "A.pbm" },
            Tile { name = "B", code = 0x02, content = File "B.pbm" },
            Tile { name = "C", code = 0x03, content = File "C.pbm" },
            Tile { name = "D", code = 0x04, content = File "D.pbm" },
            Tile { name = "E", code = 0x05, content = File "E.pbm" },
            Tile { name = "F", code = 0x06, content = File "F.pbm" },
            Tile { name = "G", code = 0x07, content = File "G.pbm" },
            Tile { name = "H", code = 0x08, content = File "H.pbm" },
            Tile { name = "I", code = 0x09, content = File "I.pbm" },
            Tile { name = "J", code = 0x0A, content = File "J.pbm" },
            Tile { name = "K", code = 0x0B, content = File "K.pbm" },
            Tile { name = "L", code = 0x0C, content = File "L.pbm" },
            Tile { name = "M", code = 0x0D, content = File "M.pbm" },
            Tile { name = "N", code = 0x0E, content = File "N.pbm" },
            Tile { name = "O", code = 0x0F, content = File "O.pbm" },
            Tile { name = "P", code = 0x10, content = File "P.pbm" },
            Tile { name = "Q", code = 0x11, content = File "Q.pbm" },
            Tile { name = "R", code = 0x12, content = File "R.pbm" },
            Tile { name = "S", code = 0x13, content = File "S.pbm" },
            Tile { name = "T", code = 0x14, content = File "T.pbm" },
            Tile { name = "U", code = 0x15, content = File "U.pbm" },
            Tile { name = "V", code = 0x16, content = File "V.pbm" },
            Tile { name = "W", code = 0x17, content = File "W.pbm" },
            Tile { name = "X", code = 0x18, content = File "X.pbm" },
            Tile { name = "Y", code = 0x19, content = File "Y.pbm" },
            Tile { name = "Z", code = 0x1A, content = File "Z.pbm" }]}

ascii :: TileMap
ascii = TileMap {
  defaultTile = Empty,
  tiles = [ Tile { name = "A", code = 0x41, content = File "A.pbm" } ]}
