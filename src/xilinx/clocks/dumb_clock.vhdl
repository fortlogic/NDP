library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VComponents.all;

entity dumb_clock is
    Port ( raw_clk         : in  STD_LOGIC;
           reset           : in  STD_LOGIC;
           stable          : out STD_LOGIC;
           main_clk        : out STD_LOGIC);
end dumb_clock;

architecture Behavioral of dumb_clock is
begin

  BUFG_clk : BUFG port map (
    I => raw_clk,
    O => main_clk);

  stable <= not reset;


end Behavioral;
