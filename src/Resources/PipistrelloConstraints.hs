requiredConstraints = Constraints {
  rawConstraints = [
      "CONFIG VCCAUX = \"3.3\"",
      "TIMESPEC \"TS_clk_in\" = PERIOD \"clk_vec_in(0)\" 20000 ps INPUT_JITTER 200 ps"
      ],
  netConstraints =
      [ SingleNet "clk_vec_in" "H17" [ NetKV "IOSTANDARD" "LVTTL" ]
      , SingleNetLocless "clk_vec_in" [ NetKV "TNM_NET" "\"clk_vec_in(0)\"" ]
      ]
  }

audioNets = Constraints {
  rawConstraints = [],
  netConstraints =
      [ SingleNet "audio_out_left" "R7"
        [ NetKV "IOSTANDARD" "LVTTL"
        , NetKV "SLEW" "SLOW"
        , NetKV "DRIVE" "8" ]
      , SingleNet "audio_out_right" "T7"
        [ NetKV "IOSTANDARD" "LVTTL"
        , NetKV "SLEW" "SLOW"
        , NetKV "DRIVE" "8" ]
      ]
  }

builtinLEDNets = Constraints {
  rawConstraints = [],
  netConstraints =
      [ SingleNet "led_hdmi_green" "V16"
        [ NetKV "IOSTANDARD" "LVTTL"
        , NetKV "SLEW" "SLOW"
        , NetKV "DRIVE" "8" ]
      , SingleNet "led_hdmi_red" "U16"
        [ NetKV "IOSTANDARD" "LVTTL"
        , NetKV "SLEW" "SLOW"
        , NetKV "DRIVE" "8" ]
      , SingleNet "led_sd_green" "A16"
        [ NetKV "IOSTANDARD" "LVTTL"
        , NetKV "SLEW" "SLOW"
        , NetKV "DRIVE" "8" ]
      , SingleNet "led_sd_red" "A15"
        [ NetKV "IOSTANDARD" "LVTTL"
        , NetKV "SLEW" "SLOW"
        , NetKV "DRIVE" "8" ]
      , SingleNet "led_usb_red" "A12"
        [ NetKV "IOSTANDARD" "LVTTL"
        , NetKV "SLEW" "SLOW"
        , NetKV "DRIVE" "8"]
      ]
  }

builtinButtonNets = Constraints {
  rawConstraints = [],
  netConstraints = [ SingleNet "button" "N14" [ NetKV "IOSTANDARD" "LVTTL" ]]
  }

sdCardSpiNets = Constraints {
  rawConstraints = [],
  netConstraints =
      [ SingleNet "sd_cs" "A2"
        [ NetKV "IOSTANDARD" "SDIO"
        , NetKV "SLEW" "FAST"
        , NetKV "DRIVE" "8"
        {- , NetFlag "PULLUP" -} ]
      , SingleNet "sd_miso" "B4"
        [ NetKV "IOSTANDARD" "SDIO"
        , NetKV "SLEW" "FAST"
        , NetKV "DRIVE" "8"
        {- , NetFlag "PULLUP" -} ]
      , SingleNet "sd_mosi" "B3"
        [ NetKV "IOSTANDARD" "SDIO"
        , NetKV "SLEW" "FAST"
        , NetKV "DRIVE" "8"
        {- , NetFlag "PULLUP" -} ]
      , SingleNet "sd_clk" "A3"
        [ NetKV "IOSTANDARD" "SDIO"
        , NetKV "SLEW" "FAST"
        , NetKV "DRIVE" "8" ]
      ]
  }

tmdsNets = Constraints {
  rawConstraints = [],
  netConstraints =
      [ BusNet "tmds_p"
        [ "T6", "U7", "U8", "U5" ]
        [ NetKV "IOSTANDARD" "TMDS_33" ]
      , BusNet "tmds_n"
        [ "V6", "V7", "V8", "V5" ]
        [ NetKV "IOSTANDARD" "TMDS_33" ]
      ]
  }

wings = Constraints {
  rawConstraints = [],
  netConstraints =
      [ BusNet "wing_a"
        [ "U18", "T17", "P17", "P16"
        , "N16", "N17", "M16", "L15"
        , "L17", "K15", "K17", "J16"
        , "H15", "H18", "F18", "D18" ]
        [ NetKV "IOSTANDARD" "LVTTL"
        , NetKV "DRIVE" "8"
        , NETKV "SLEW" "FAST"
        , NetFlag "PULLUP" ]
      ]
  }
