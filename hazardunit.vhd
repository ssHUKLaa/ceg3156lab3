library ieee;
use ieee.std_logic_1164.all;

entity hazardunit is
    port (
        MemRead : IN std_logic;
        RtID, RtIF, RsIF : IN std_logic_vector(4 downto 0);
        stall : OUT std_logic
    );
end hazardunit;

architecture basic of hazardunit IS

    SIGNAL rteqrt, rteqrs, anyeq : std_logic;
begin
    rteqrt <= NOT (RtID(4) XOR RtIF(4)) AND NOT (RtID(3) XOR RtIF(3)) AND NOT (RtID(2) XOR RtIF(2)) AND NOT (RtID(1) XOR RtIF(1)) AND NOT (RtID(0) XOR RtIF(0));
    rteqrs <= NOT (RtID(4) XOR RsIF(4)) AND NOT (RtID(3) XOR RsIF(3)) AND NOT (RtID(2) XOR RsIF(2)) AND NOT (RtID(1) XOR RsIF(1)) AND NOT (RtID(0) XOR RsIF(0));

    anyeq <= rteqrt or rteqrs;
    stall <= MemRead and anyeq and '0';
    
end basic;