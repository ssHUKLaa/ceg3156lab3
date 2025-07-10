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
            Zero        : in std_logic;
            RegDst		: out std_logic;
            ALUSrc		: out std_logic;
            MemtoReg		: out std_logic;
            RegWrite		: out std_logic;
            MemRead		: out std_logic;
            MemWrite		: out std_logic;
            Branch		: out std_logic;
            BNE : out std_logic;
            Jump : out std_logic;
            Flush : out std_logic;
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

    component hazardunit is
        port (
            MemRead : IN std_logic;
            RtID, RtIF, RsIF : IN std_logic_vector(4 downto 0);
            stall : OUT std_logic
        );
    end component;

    SIGNAL newPCval, PC_SIG, instrOut, incPC, incPCGated, sgnextinstr, shlsgnextinstr, readReg1, readReg2, branchaddr : std_logic_vector(31 downto 0);
    SIGNAL FlushedOpcode : std_logic_vector(5 downto 0);
    SIGNAL hduStall, hduGatedRegDst, hduGatedALUSrc, hduGatedMemtoReg, hduGatedRegWrite, hduGatedMemRead, hduGatedMemWrite, hduGatedBranch, hduGatedJmp : std_logic;
    SIGNAL Flush, gatedFlush, readregseq, PCSrc : std_logic;
    SIGNAL RegDst, ALUSrc, MemtoReg, RegWrite, MemRead, MemWrite, Branch, Jump : std_logic;
    SIGNAL hduGatedALUOp, ALUOp : std_logic_vector(1 downto 0);
    SIGNAL EXreadReg1, EXreadReg2, EXsgnextinstr : std_logic_vector(31 downto 0);
    SIGNAL EXinstruction : std_logic_vector(25 downto 11);

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

    PCSrc <= Branch and readregseq;

    mux_2to1_32bit_inst: entity work.mux_2to1_32bit
    port map (
      sel   => PCSrc,
      d_in1 => incPC,
      d_in2 => branchaddr,
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

    IF_ID_Flush: entity work.enARdFF_2
		port map (
		  i_resetBar => GReset,
		  i_d        => Flush,
		  i_enable   => '1',
		  i_clock    => GClock,
		  o_q        => gatedFlush,
		  o_qBar     => open
		);
    
    instruction_inst: entity work.instruction_rom
        PORT MAP (
           address =>  PC_SIG(9 downto 2),
           clock => GClock,
           q => instrOut
        );

    FlushedOpcode <= (instrOut(31) AND NOT gatedFlush) & (instrOut(30) AND NOT gatedFlush) & (instrOut(29) AND NOT gatedFlush) & (instrOut(28) AND NOT gatedFlush) & (instrOut(27) AND NOT gatedFlush) & (instrOut(26) AND NOT gatedFlush);

    hazardunit_inst: entity work.hazardunit
    port map (
      MemRead => MemRead,
      RtID    => RtID,
      RtIF    => InstructionOut(20 downto 16),
      RsIF    => InstructionOut(25 downto 21),
      stall   => hduStall
    );

    control_unit_inst: entity work.control_unit
    port map (
      opcode   => FlushedOpcode,
      Zero     => test,
      RegDst   => RegDst,
      ALUSrc   => ALUSrc,
      MemtoReg => MemtoReg,
      RegWrite => RegWrite,
      MemRead  => MemRead,
      MemWrite => MemWrite,
      Branch   => Branch,
      BNE      => open,
      Jump     => Jump,
      Flush => Flush,
      ALUOp    => ALUOp
    );

    hduGatedRegDst <= RegDst AND NOT (hduStall or Flush);
    hduGatedALUSrc <= ALUSrc AND NOT (hduStall or Flush);
    hduGatedMemtoReg <= MemtoReg AND NOT (hduStall or Flush);
    hduGatedRegWrite <= RegWrite AND NOT (hduStall or Flush);
    hduGatedMemRead <= MemRead AND NOT (hduStall or Flush);
    hduGatedMemWrite <= MemWrite AND NOT (hduStall or Flush);
    hduGatedBranch <= Branch AND NOT (hduStall or Flush);
    hduGatedALUOp(1) <= ALUOP(1) AND NOT (hduStall or Flush);
    hduGatedALUOp(0) <= ALUOP(0) AND NOT (hduStall or Flush);

    EX_RegDst_DFF: entity work.enARdFF_2
		port map (
		  i_resetBar => GReset,
		  i_d        => hduGatedRegDst,
		  i_enable   => '1',
		  i_clock    => GClock,
		  o_q        => EXRegDst,
		  o_qBar     => open
		);

    EX_ALUSrc_DFF: entity work.enARdFF_2
		port map (
		  i_resetBar => GReset,
		  i_d        => hduGatedALUSrc,
		  i_enable   => '1',
		  i_clock    => GClock,
		  o_q        => EXALUSrc,
		  o_qBar     => open
		);

    EX_MemtoReg_DFF: entity work.enARdFF_2
		port map (
		  i_resetBar => GReset,
		  i_d        => hduGatedMemtoReg,
		  i_enable   => '1',
		  i_clock    => GClock,
		  o_q        => EXMemtoReg,
		  o_qBar     => open
		);

    EX_MemRead_DFF: entity work.enARdFF_2
		port map (
		  i_resetBar => GReset,
		  i_d        => hduGatedMemRead,
		  i_enable   => '1',
		  i_clock    => GClock,
		  o_q        => EXMemRead,
		  o_qBar     => open
		);

    EX_RegWrite_DFF: entity work.enARdFF_2
		port map (
		  i_resetBar => GReset,
		  i_d        => hduGatedRegWrite,
		  i_enable   => '1',
		  i_clock    => GClock,
		  o_q        => EXRegWrite,
		  o_qBar     => open
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
      sum         => branchaddr,
      carryOut    => open,
      zeroOut     => open,
      overflowOut => open
    );

    readregseq <= 
    not (readReg1(31) xor readReg2(31)) and
    not (readReg1(30) xor readReg2(30)) and
    not (readReg1(29) xor readReg2(29)) and
    not (readReg1(28) xor readReg2(28)) and
    not (readReg1(27) xor readReg2(27)) and
    not (readReg1(26) xor readReg2(26)) and
    not (readReg1(25) xor readReg2(25)) and
    not (readReg1(24) xor readReg2(24)) and
    not (readReg1(23) xor readReg2(23)) and
    not (readReg1(22) xor readReg2(22)) and
    not (readReg1(21) xor readReg2(21)) and
    not (readReg1(20) xor readReg2(20)) and
    not (readReg1(19) xor readReg2(19)) and
    not (readReg1(18) xor readReg2(18)) and
    not (readReg1(17) xor readReg2(17)) and
    not (readReg1(16) xor readReg2(16)) and
    not (readReg1(15) xor readReg2(15)) and
    not (readReg1(14) xor readReg2(14)) and
    not (readReg1(13) xor readReg2(13)) and
    not (readReg1(12) xor readReg2(12)) and
    not (readReg1(11) xor readReg2(11)) and
    not (readReg1(10) xor readReg2(10)) and
    not (readReg1(9) xor readReg2(9)) and
    not (readReg1(8) xor readReg2(8)) and
    not (readReg1(7) xor readReg2(7)) and
    not (readReg1(6) xor readReg2(6)) and
    not (readReg1(5) xor readReg2(5)) and
    not (readReg1(4) xor readReg2(4)) and
    not (readReg1(3) xor readReg2(3)) and
    not (readReg1(2) xor readReg2(2)) and
    not (readReg1(1) xor readReg2(1)) and
    not (readReg1(0) xor readReg2(0));

    ID_EX_RegData1: FOR i in 0 to 31 GENERATE 
        enardff_2_inst: entity work.enARdFF_2
		port map (
		  i_resetBar => GReset,
		  i_d        => readReg1(i),
		  i_enable   => '1',
		  i_clock    => GClock,
		  o_q        => EXreadReg1(i),
		  o_qBar     => open
		);
	end generate;

    ID_EX_RegData2: FOR i in 0 to 31 GENERATE 
        enardff_2_inst: entity work.enARdFF_2
		port map (
		  i_resetBar => GReset,
		  i_d        => readReg2(i),
		  i_enable   => '1',
		  i_clock    => GClock,
		  o_q        => EXreadReg2(i),
		  o_qBar     => open
		);
	end generate;

    ID_EX_sgnext: FOR i in 0 to 31 GENERATE 
        enardff_2_inst: entity work.enARdFF_2
		port map (
		  i_resetBar => GReset,
		  i_d        => sgnextinstr(i),
		  i_enable   => '1',
		  i_clock    => GClock,
		  o_q        => EXsgnextinstr(i),
		  o_qBar     => open
		);
	end generate;

    ID_EX_instruction: FOR i in 11 to 25 GENERATE 
        enardff_2_inst: entity work.enARdFF_2
		port map (
		  i_resetBar => GReset,
		  i_d        => InstructionOut(i),
		  i_enable   => '1',
		  i_clock    => GClock,
		  o_q        => EXinstruction(i),
		  o_qBar     => open
		);
	end generate;

    
    


end basic;

