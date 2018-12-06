{ name = "VideoDemos"
, targets =
    [ { target = "HamsterDVIDPlusTMDS"
      , entities = [ "tmds_encoder" ]
      , source = "Main.hs"
      , hdl =
          [ { language = "vhdl"
            , files =
                [ "HamsterDVIDPlusTMDS.vhdl"
                , "simple_dvid.vhdl"
                , "vga_clocking.vdhl"
                , "vga_gen.vhdl"
                ]
            }
          ]
      }
    ]
}