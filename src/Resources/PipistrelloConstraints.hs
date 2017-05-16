requiredConstraints = Constraints {
  rawConstraints = [
      "CONFIG VCCAUX = \"3.3\"",
      "TIMESPEC \"TS_clk_in\" = PERIOD \"clk_in(0)\" 20000 ps INPUT_JITTER 200 ps"
      ],
  netConstraints = [
      ("clk_in(0)", [NetLoc "H17", NetKV "IOSTANDARD" "LVTTL"]),
      ("clk_in(0)", [NetKV "TNM_NET" "\"clk_in(0)\""])
      ]
  }

audioNets = Constraints {
  rawConstraints = [],
  netConstraints = [
      ("audio_out_left(0)", [ NetLoc "R7",
                           NetKV "IOSTANDARD" "LVTTL",
                           NetKV "SLEW" "SLOW",
                           NetKV "DRIVE" "8" ]),
      ("audio_out_right(0)", [ NetLoc "T7",
                            NetKV "IOSTANDARD" "LVTTL",
                            NetKV "SLEW" "SLOW",
                            NetKV "DRIVE" "8" ])
      ]
  }

builtinLEDNets = Constraints {
  rawConstraints = [],
  netConstraints = [
      ("led_hdmi_green(0)", [ NetLoc "V16",
                           NetKV "IOSTANDARD" "LVTTL",
                           NetKV "SLEW" "SLOW",
                           NetKV "DRIVE" "8"]),
      ("led_hdmi_red(0)", [ NetLoc "U16",
                         NetKV "IOSTANDARD" "LVTTL",
                         NetKV "SLEW" "SLOW",
                         NetKV "DRIVE" "8"]),
      ("led_sd_green(0)", [ NetLoc "A16",
                         NetKV "IOSTANDARD" "LVTTL",
                         NetKV "SLEW" "SLOW",
                         NetKV "DRIVE" "8"]),
      ("led_sd_red(0)", [ NetLoc "A15",
                       NetKV "IOSTANDARD" "LVTTL",
                       NetKV "SLEW" "SLOW",
                       NetKV "DRIVE" "8"]),
      ("led_usb_red(0)", [ NetLoc "A12",
                        NetKV "IOSTANDARD" "LVTTL",
                        NetKV "SLEW" "SLOW",
                        NetKV "DRIVE" "8"])
      ]
  }

builtinButtonNets = Constraints {
  rawConstraints = [],
  netConstraints = [ ("button(0)", [ NetLoc "N14", NetKV "IOSTANDARD" "LVTTL" ])]
  }

tmdsNets = Constraints {
  rawConstraints = [],
  netConstraints = [
      ("tmds_p(0)", [ NetLoc "T6", NetKV "IOSTANDARD" "TMDS_33" ]),
      ("tmds_p(1)", [ NetLoc "U7", NetKV "IOSTANDARD" "TMDS_33" ]),
      ("tmds_p(2)", [ NetLoc "U8", NetKV "IOSTANDARD" "TMDS_33" ]),
      ("tmds_p(3)", [ NetLoc "U5", NetKV "IOSTANDARD" "TMDS_33" ]),
      ("tmds_n(0)", [ NetLoc "V6", NetKV "IOSTANDARD" "TMDS_33" ]),
      ("tmds_n(1)", [ NetLoc "V7", NetKV "IOSTANDARD" "TMDS_33" ]),
      ("tmds_n(2)", [ NetLoc "V8", NetKV "IOSTANDARD" "TMDS_33" ]),
      ("tmds_n(3)", [ NetLoc "V5", NetKV "IOSTANDARD" "TMDS_33" ])
      ]
  }
