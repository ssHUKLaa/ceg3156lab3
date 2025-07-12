LIBRARY ieee;
USE ieee.std_logic_1164.ALL;


entity Forwarding_unit is
    port (
        ID_EX_rs        : in std_logic_vector(4 downto 0);
        ID_EX_rt        : in std_logic_vector(4 downto 0);
        EX_MEM_RegWrite : in std_logic;
        EX_MEM_rd       : in std_logic_vector(4 downto 0);
        MEM_WB_RegWrite : in std_logic;
        MEM_WB_rd       : in std_logic_vector(4 downto 0);
        ForwardA        : out std_logic_vector(1 downto 0);
        ForwardB        : out std_logic_vector(1 downto 0)
    );
end forwarding_unit;

architecture basic of Forwarding_unit is

    component equality_Comparator_5bit IS
        PORT (
            A, B : IN STD_LOGIC_VECTOR(4 downto 0);
            isEqual : OUT STD_LOGIC
        );
    END component;

begin

    ForwardA <= "10" when (EX_MEM_RegWrite = '1' and EX_MEM_rd /= "00000" and EX_MEM_rd = ID_EX_rs) else
                "01" when (MEM_WB_RegWrite = '1' and MEM_WB_rd /= "00000" and MEM_WB_rd = ID_EX_rs) else
                "00";

    ForwardB <= "10" when (EX_MEM_RegWrite = '1' and EX_MEM_rd /= "00000" and EX_MEM_rd = ID_EX_rt) else
                "01" when (MEM_WB_RegWrite = '1' and MEM_WB_rd /= "00000" and MEM_WB_rd = ID_EX_rt) else
                "00";

end basic;
