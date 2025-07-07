library ieee;
use ieee.std_logic_1164.all;

entity pipelinedproc is
    port(
        GClock, GReset : IN std_logic;
        ValueSelect, InstrSelect : IN std_logic_vector(1 downto 0);
        MuxOut : OUT std_logic_vector(7 downto 0);
        InstructionOut : OUT std_logic_vector(31 downto 0);
        BranchOut, ZeroOut, MemWriteOut, RegWriteOut : OUT std_logic
    );
end pipelinedproc;

architecture basic of pipelinedproc is

    component enARdFF_2 IS
        PORT(
            i_resetBar	: IN	STD_LOGIC;
            i_d		: IN	STD_LOGIC;
            i_enable	: IN	STD_LOGIC;
            i_clock		: IN	STD_LOGIC;
            o_q, o_qBar	: OUT	STD_LOGIC);
    END component;

    component reg_file is
        port (
            clk         : in  std_logic;
            resetBar    : in  std_logic;
            reg_write   : in  std_logic;
            read_reg1   : in  std_logic_vector(4 downto 0);
            read_reg2   : in  std_logic_vector(4 downto 0);
            write_reg   : in  std_logic_vector(4 downto 0);
            write_data  : in  std_logic_vector(31 downto 0);
            read_data1  : out std_logic_vector(31 downto 0);
            read_data2  : out std_logic_vector(31 downto 0)
        );
    end component;

    component data_ram IS
        PORT
        (
            aclr		: IN STD_LOGIC  := '0';
            address		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
            clock		: IN STD_LOGIC  := '1';
            data		: IN STD_LOGIC_VECTOR (31 DOWNTO 0);
            rden		: IN STD_LOGIC  := '1';
            wren		: IN STD_LOGIC ;
            q		: OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
        );
    END component;

    component instruction_rom IS
        PORT
        (
            address		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
            clock		: IN STD_LOGIC  := '1';
            q		: OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
        );
    END component;

    component ALU_32bit is
        PORT (
            A, B : IN std_logic_vector(31 downto 0);
            sel : IN std_logic_vector(2 downto 0);
            ALU_res : OUT std_logic_vector(31 downto 0);
            Zero : OUT std_logic
        );
    end component;

    component CLA_32bit IS
        PORT (
            a, b      : IN  std_logic_vector(31 downto 0);
            cin       : IN  std_logic;
            sum       : OUT std_logic_vector(31 downto 0);
            carryOut  : OUT std_logic;
            zeroOut   : OUT std_logic;
            overflowOut : OUT std_logic
        );
    END component;

    component ALU_Control is
        port (
            ALUOP : IN std_logic_vector(1 downto 0);
            funct : IN std_logic_vector(5 downto 0);
            Opr : OUT std_logic_vector(2 downto 0)
        );
    end component;

    component control_unit is
        port(
            opcode		: in std_logic_vector(5 downto 0);
            RegDst		: out std_logic;
            ALUSrc		: out std_logic;
            MemtoReg		: out std_logic;
            RegWrite		: out std_logic;
            MemRead		: out std_logic;
            MemWrite		: out std_logic;
            Branch		: out std_logic;
            BNE : out std_logic;
            Jump : out std_logic;
            ALUOp			: out std_logic_vector(1 downto 0)
        );
    end component;

    component mux_2to1_32bit IS
        PORT(
            sel     : IN  STD_LOGIC;                              -- Select input
            d_in1   : IN  STD_LOGIC_VECTOR(31 DOWNTO 0);          -- 32-bit Data input 1
            d_in2   : IN  STD_LOGIC_VECTOR(31 DOWNTO 0);          -- 32-bit Data input 2
            d_out   : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)           -- 32-bit Data output
        );
    END component;

    SIGNAL newPCval, PC_SIG, instrOut, incPC, incPCGated, sgnextinstr, shlsgnextinstr, readReg1, readReg2, jmpaddr : std_logic_vector(31 downto 0);

begin

    GEN_PC: FOR i IN 0 TO 31 GENERATE
		enardff_2_inst: entity work.enARdFF_2
		port map (
		  i_resetBar => GReset,
		  i_d        => newPCval(i),
		  i_enable   => '1',
		  i_clock    => GClock,
		  o_q        => PC_SIG(i),
		  o_qBar     => open
		);
	end generate;

    inc_pc: entity work.CLA_32bit
	port map (
        a           => PC_SIG,
        b           => "00000000000000000000000000000100",
        cin         => '0',
        Sum         => incPC,
        CarryOut    => open,
        zeroOut     => open,
        OverFlowOut => open
	);

    mux_2to1_32bit_inst: entity work.mux_2to1_32bit
    port map (
      sel   => PCSrc,
      d_in1 => incPC,
      d_in2 => jmpaddr,
      d_out => newPCval
    );

    GEN_Gated_INCPC: FOR i in 0 to 31 GENERATE 
        enardff_2_inst: entity work.enARdFF_2
		port map (
		  i_resetBar => GReset,
		  i_d        => incPC(i),
		  i_enable   => '1',
		  i_clock    => GClock,
		  o_q        => incPCGated(i),
		  o_qBar     => open
		);
	end generate;
    
    instruction_inst: entity work.instruction_rom
        PORT MAP (
           address =>  PC_SIG(9 downto 2),
           clock => GClock,
           q => instrOut
        );

    control_unit_inst: entity work.control_unit
    port map (
      opcode   => instrOut(31 downto 26),
      RegDst   => RegDst,
      ALUSrc   => ALUSrc,
      MemtoReg => MemtoReg,
      RegWrite => RegWrite,
      MemRead  => MemRead,
      MemWrite => MemWrite,
      Branch   => Branch,
      BNE      => BNE,
      Jump     => Jump,
      ALUOp    => ALUOp
    );

    reg_file_inst: entity work.reg_file
    port map (
      clk        => GClock,
      resetBar   => GReset,
      reg_write  => RegWrite,
      read_reg1  => InstructionOut(25 downto 21),
      read_reg2  => InstructionOut(20 downto 16),
      write_reg  => test,
      write_data => test,
      read_data1 => readReg1,
      read_data2 => readReg2
    );

    sgnextinstr <= (others => InstructionOut(15)) & instrOut(15 downto 0);
    
    shlsgnextinstr <= sgnextinstr(29 downto 0) & "00";

    cla_32bit_inst: entity work.CLA_32bit
    port map (
      a           => shlsgnextinstr,
      b           => incPCGated,
      cin         => '0',
      sum         => jmpaddr,
      carryOut    => open,
      zeroOut     => open,
      overflowOut => open
    );


    


end basic;

