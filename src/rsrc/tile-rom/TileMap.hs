cbm = TileMap {
  defaultTile = Empty,
  tiles = [ Tile { name = "A", code = 0x01, content = File "A.pbm" },
            Tile { name = "B", code = 0x02, content = File "B.pbm" } ]}

ascii = TileMap {
  defaultTile = Empty,
  tiles = [ Tile { name = "A", code = 0x41, content = File "A.pbm" } ]}
