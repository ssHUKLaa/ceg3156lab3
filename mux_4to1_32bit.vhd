LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY mux_4to1_32bit IS
    PORT (
        sel     : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);               -- 2-bit select signal
        d_in0   : IN  STD_LOGIC_VECTOR(31 DOWNTO 0);              -- 32-bit input 0
        d_in1   : IN  STD_LOGIC_VECTOR(31 DOWNTO 0);              -- 32-bit input 1
        d_in2   : IN  STD_LOGIC_VECTOR(31 DOWNTO 0);              -- 32-bit input 2
        d_in3   : IN  STD_LOGIC_VECTOR(31 DOWNTO 0);              -- 32-bit input 3
        d_out   : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)               -- 32-bit output
    );
END mux_4to1_32bit;

ARCHITECTURE structural OF mux_4to1_32bit IS

    COMPONENT mux_2to1_32bit
        PORT (
            sel     : IN  STD_LOGIC;
            d_in1   : IN  STD_LOGIC_VECTOR(31 DOWNTO 0);
            d_in2   : IN  STD_LOGIC_VECTOR(31 DOWNTO 0);
            d_out   : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;

    SIGNAL mux_out0, mux_out1 : STD_LOGIC_VECTOR(31 DOWNTO 0);

BEGIN

    -- First layer: select between input pairs
    mux0: mux_2to1_32bit PORT MAP(
        sel    => sel(0),
        d_in1  => d_in0,
        d_in2  => d_in1,
        d_out  => mux_out0
    );

    mux1: mux_2to1_32bit PORT MAP(
        sel    => sel(0),
        d_in1  => d_in2,
        d_in2  => d_in3,
        d_out  => mux_out1
    );

    -- Second layer: select final output
    mux2: mux_2to1_32bit PORT MAP(
        sel    => sel(1),
        d_in1  => mux_out0,
        d_in2  => mux_out1,
        d_out  => d_out
    );

END structural;
