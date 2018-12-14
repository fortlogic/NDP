{ name = "VideoDemos"
, targets =
    [ { target = "HamsterDVIDPlusTMDS"
      , entities = [ "tmds_encoder" ]
      , source = "Main.hs"
      , constraints =
        [ { name = "Required"
          , config = [ { key = "VCCAUX", value = "\"3.3\"" } ]
          , timespec =
            [ { name = "TS_clk_in"
              , net = "clk_vec_in(0)"
              , period = { measure = 20000, unit = "ps" }
              , inputJitter = { measure = 200, unit = "ps" }
              }
            ]
          , nets =
            [ { name = "clk_vec_in"
              , location = "H17"
              , params =
                  [ < Option = { key = "IOSTANDARD", value = "LVTTL" } | Flag : Text >
                  , < Option = { key = "TNM_NET", value = "\"clk_vec_in(0)\""} | Flag : Text > ]
              }
            ]
          }
        -- , "tmdsNets"
        ]
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