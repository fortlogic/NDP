----------------------------------------------------------------------------------
-- Company:
-- Engineer:
--
-- Create Date:    05:39:11 09/14/2016
-- Design Name:
-- Module Name:    simple_dvid - Behavioral
-- Project Name:
-- Target Devices:
-- Tool versions:
-- Description:
--
-- Dependencies:
--
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
library UNISIM;
use UNISIM.VComponents.all;

-- from tmds_encoder
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.minimaldvidplustmds_types.all;

entity simple_dvid is
    Port ( clk                   : in  STD_LOGIC;
           hsync,  vsync,  blank : in  STD_LOGIC;
           red,    green,  blue  : in  STD_LOGIC_VECTOR (7 downto 0);
           hdmi_p, hdmi_n        : out STD_LOGIC_VECTOR (3 downto 0));
end simple_dvid;

architecture Behavioral of simple_dvid is
   type a_symbols     is array (0 to 3) of std_logic_vector(9 downto 0);
   type a_colours     is array (0 to 2) of std_logic_vector(7 downto 0);
   type a_ctls        is array (0 to 2) of std_logic_vector(1 downto 0);
   type a_output_bits is array (0 to 3) of std_logic_vector(1 downto 0);

   signal symbols        : a_symbols     := (others => (others => '0'));
   signal high_speed_sr  : a_symbols     := (others => (others => '0'));
   signal colours        : a_colours     := (others => (others => '0'));
   signal ctls           : a_ctls        := (others => (others => '0'));
   signal output_bits    : a_output_bits := (others => (others => '0'));

   -- Controlling when the transfers into the high speed domain occur
   signal latch_high_speed : std_logic_vector(4 downto 0) := "00001";

   -- The signals from the DDR outputs to the output buffers
   signal serial_outputs : std_logic_vector(3 downto 0);

   -- For generating the x5 clocks
   signal clk_x5,  clk_x5_unbuffered  : std_logic;
   signal clk_feedback    : std_logic;

   signal blank_bool : boolean;

begin
   ctls(0) <= vsync & hsync; -- syncs are set in the channel 0 CTL periods

   colours(0) <= blue;
   colours(1) <= green;
   colours(2) <= red;

   symbols(3) <= "0000011111"; -- the clock channel symbol is static

   blank_bool <= TRUE when blank = '1' else FALSE;

   tmds_gen:
   for i in 0 to 2 generate
     tmds_encoder_1: entity work.tmds_encoder
       port map (
         blank_en      => blank_bool,
         ctl_in        => ctls(i),
         px_in         => unsigned(colours(i)),
         HDMI4000      => clk,
         HDMI4000_rstn => '1',
         tmds_out      => symbols(i));
   end generate tmds_gen;

process(clk_x5)
   begin
      ---------------------------------------------------------------
      -- Now take the 10-bit words and take it into the high-speed
      -- clock domain once every five cycles.
      --
      -- Then send out two bits every clock cycle using DDR output
      -- registers.
      ---------------------------------------------------------------
      if rising_edge(clk_x5) then
         for i in 0 to 3 loop
            output_bits(i)  <= high_speed_sr(i)(1 downto 0);
            if latch_high_speed(0) = '1' then
               high_speed_sr(i) <= symbols(i);
            else
               high_speed_sr(i) <= "00" & high_speed_sr(i)(9 downto 2);
            end if;
         end loop;
         latch_high_speed <= latch_high_speed(0) & latch_high_speed(4 downto 1);
      end if;
   end process;

g1:   for i in 0 to 3 generate
   --------------------------------------------------------
   -- Convert the TMDS codes into a serial stream, two bits
   -- at a time using a DDR register
   --------------------------------------------------------
      to_serial: ODDR2
         generic map(DDR_ALIGNMENT => "C0", INIT => '0', SRTYPE => "ASYNC")
         port map (C0 => clk_x5,  C1 => not clk_x5, CE => '1', R => '0', S => '0',
                   D0 => output_bits(i)(0), D1 => output_bits(i)(1), Q => serial_outputs(i));
      OBUFDS_c0  : OBUFDS port map ( O  => hdmi_p(i), OB => hdmi_n(i), I => serial_outputs(i));
   end generate;

   ------------------------------------------------------------------
   -- Use a PLL to generate a x5 clock, which is used to drive
   -- the DDR registers.This allows 10 bits to be sent for every
   -- pixel clock
   ------------------------------------------------------------------
PLL_BASE_inst : PLL_BASE generic map (
      CLKFBOUT_MULT => 10,
      CLKOUT0_DIVIDE => 2,
      CLKOUT0_PHASE => 0.0,   -- Output 5x original frequency
      CLK_FEEDBACK => "CLKFBOUT",
      CLKIN_PERIOD => 25.0,
      DIVCLK_DIVIDE => 1
   ) port map (
      CLKFBOUT => clk_feedback,
      CLKOUT0  => clk_x5_unbuffered,
      CLKFBIN  => clk_feedback,
      CLKIN    => clk,
      RST      => '0'
   );

BUFG_pclkx5  : BUFG port map ( I => clk_x5_unbuffered,  O => clk_x5);

end Behavioral;
