requiredConstraints = Constraints {
  rawConstraints = [
      "CONFIG VCCAUX = \"3.3\"",
      "TIMESPEC \"TS_clk_in\" = PERIOD \"clk_in\" 20000 ps INPUT_JITTER 200 ps"
      ],
  netConstraints = [
      ("clk_in", [("LOC","\"H17\""), ("IOSTANDARD", "LVTTL")]),
      ("clk_in", [("TNM_NET", "\"clk_in\"")])
      ]
  }

tmdsNets = Constraints {
  rawConstraints = [],
  netConstraints = [
      ("tmds_p(0)", [("LOC", "\"T6\""), ("IOSTANDARD", "TMDS_33")]),
      ("tmds_p(1)", [ ("LOC", "\"U7\""), ("IOSTANDARD", "TMDS_33") ]),
      ("tmds_p(2)", [ ("LOC", "\"U8\""), ("IOSTANDARD", "TMDS_33") ]),
      ("tmds_p(3)", [ ("LOC", "\"U5\""), ("IOSTANDARD", "TMDS_33") ]),
      ("tmds_n(0)", [ ("LOC", "\"V6\""), ("IOSTANDARD", "TMDS_33") ]),
      ("tmds_n(1)", [ ("LOC", "\"V7\""), ("IOSTANDARD", "TMDS_33") ]),
      ("tmds_n(2)", [ ("LOC", "\"V8\""), ("IOSTANDARD", "TMDS_33") ]),
      ("tmds_n(3)", [ ("LOC", "\"V5\""), ("IOSTANDARD", "TMDS_33") ])
      ]
  }
