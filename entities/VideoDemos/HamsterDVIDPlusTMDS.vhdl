library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

Entity HamsterDVIDPlusTMDS is
    Port ( clk_vec_in         : in  STD_LOGIC_VECTOR(0 downto 0);

           tmds_p : out  STD_LOGIC_VECTOR(3 downto 0);
           tmds_n : out  STD_LOGIC_VECTOR(3 downto 0));
end HamsterDVIDPlusTMDS;

architecture Behavioral of HamsterDVIDPlusTMDS is

   COMPONENT vga_gen
   PORT(
      clk50           : IN std_logic;
      pixel_clock     : OUT std_logic;
      red_p           : OUT std_logic_vector(7 downto 0);
      green_p         : OUT std_logic_vector(7 downto 0);
      blue_p          : OUT std_logic_vector(7 downto 0);
      blank           : OUT std_logic;
      hsync           : OUT std_logic;
      vsync           : OUT std_logic
      );
   END COMPONENT;


   COMPONENT simple_dvid
   PORT(
      clk : IN std_logic;
      blank : IN std_logic;
      hsync : IN std_logic;
      vsync : IN std_logic;
      red : IN std_logic_vector(7 downto 0);
      green : IN std_logic_vector(7 downto 0);
      blue : IN std_logic_vector(7 downto 0);
      hdmi_p : OUT std_logic_vector(3 downto 0);
      hdmi_n : OUT std_logic_vector(3 downto 0)
      );
   END COMPONENT;

   signal pixel_clock     : std_logic;

   signal red_p   : std_logic_vector(7 downto 0);
   signal green_p : std_logic_vector(7 downto 0);
   signal blue_p  : std_logic_vector(7 downto 0);
   signal blank   : std_logic;
   signal hsync   : std_logic;
   signal vsync   : std_logic;

   signal clk_in : std_logic;

begin

  clk_in <= clk_vec_in(0);

---------------------------------------
-- Generate a 800x600 VGA test pattern
---------------------------------------
Inst_vga_gen: vga_gen PORT MAP(
      clk50 => clk_in,
      pixel_clock     => pixel_clock,
      red_p           => red_p,
      green_p         => green_p,
      blue_p          => blue_p,
      blank           => blank,
      hsync           => hsync,
      vsync           => vsync
   );

---------------------------------------------------
-- Convert 9 bits of the VGA signals to the DVI-D/TMDS output
---------------------------------------------------
Inst_MinimalDVID_encoder: simple_dvid PORT MAP(
      clk    => pixel_clock,
      blank  => blank,
      hsync  => hsync,
      vsync  => vsync,
      red    => red_p,
      green  => green_p,
      blue   => blue_p,
      hdmi_p => tmds_p,
      hdmi_n => tmds_n
   );

end Behavioral;
