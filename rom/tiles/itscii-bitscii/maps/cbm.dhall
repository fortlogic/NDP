{ name = "cbm"
, size = 256
, default = < Fill = 0 | File : Text >
, tiles =
  let mkLetter : ∀( l : Text ) → ∀( c : Natural ) →
                   { code : Natural, data : < Fill : Natural | File : Text > }
               = λ( l : Text ) → λ( c : Natural ) →
                   { code = c, data = < Fill : Natural | File = l ++ ".pbm" > }
  in
    [ { code = 0x00, data = < Fill = 0 | File : Text > }
    , mkLetter "A" 0x01
    , mkLetter "B" 0x02
    , mkLetter "C" 0x03
    , mkLetter "D" 0x04
    , mkLetter "E" 0x05
    , mkLetter "F" 0x06
    , mkLetter "G" 0x07
    , mkLetter "H" 0x08
    , mkLetter "I" 0x09
    , mkLetter "J" 0x0A
    , mkLetter "K" 0x0B
    , mkLetter "L" 0x0C
    , mkLetter "M" 0x0D
    , mkLetter "N" 0x0E
    , mkLetter "O" 0x0F
    , mkLetter "P" 0x10
    , mkLetter "Q" 0x11
    , mkLetter "R" 0x12
    , mkLetter "S" 0x13
    , mkLetter "T" 0x14
    , mkLetter "U" 0x15
    , mkLetter "V" 0x16
    , mkLetter "W" 0x17
    , mkLetter "X" 0x18
    , mkLetter "Y" 0x19
    , mkLetter "Z" 0x1A
    ] }