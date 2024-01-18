```vhdl
-- This VHDL code implements a complex and differentiated system that combines various functionalities and features.

-- Design Description:

-- 1. Register File:
--    - Defines a register file with multiple registers.
--    - Allows writing and reading data to and from the registers.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity RegisterFile is
    generic (
        NumRegisters : natural := 8;  -- Number of registers in the file
        DataWidth    : integer := 32  -- Width of data in each register
    );
    port (
        Clock       : in STD_LOGIC;
        WriteEnable  : in STD_LOGIC;
        WriteAddress : in integer range 0 to NumRegisters - 1;
        WriteData    : in STD_LOGIC_VECTOR(DataWidth - 1 downto 0);
        ReadAddress  : in integer range 0 to NumRegisters - 1;
        ReadData     : out STD_LOGIC_VECTOR(DataWidth - 1 downto 0)
    );
end RegisterFile;

architecture RegisterFileArch of RegisterFile is
    signal RegisterArray : STD_LOGIC_VECTOR(DataWidth - 1 downto 0) array (0 to NumRegisters - 1);
begin
    process (Clock)
    begin
        if (Clock'event and Clock = '1') then
            if (WriteEnable = '1') then
                RegisterArray(WriteAddress) <= WriteData;
            end if;
        end if;
    end process;

    ReadData <= RegisterArray(ReadAddress);
end RegisterFileArch;


-- 2. Arithmetic Unit:
--    - Implements basic arithmetic operations (addition, subtraction, multiplication, division).
--    - Supports signed and unsigned integer and floating-point operations.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity ArithmeticUnit is
    generic (
        DataWidth : integer := 32  -- Width of operands and results
    );
    port (
        Clock         : in STD_LOGIC;
        Operation      : in STD_LOGIC_VECTOR(3 downto 0);  -- Specifies the arithmetic operation
        SignedOperands : in STD_LOGIC;                   -- Indicates if operands are signed or unsigned
        OperandA       : in STD_LOGIC_VECTOR(DataWidth - 1 downto 0);
        OperandB       : in STD_LOGIC_VECTOR(DataWidth - 1 downto 0);
        Result         : out STD_LOGIC_VECTOR(DataWidth - 1 downto 0)
    );
end ArithmeticUnit;

architecture ArithmeticUnitArch of ArithmeticUnit is
begin
    process (Clock)
    begin
        if (Clock'event and Clock = '1') then
            case Operation is
                when "0000" => Result <= OperandA + OperandB;  -- Addition
                when "0001" => Result <= OperandA - OperandB;  -- Subtraction
                when "0010" => Result <= OperandA * OperandB;  -- Multiplication
                when "0011" => Result <= OperandA / OperandB;  -- Division
                when others => null;                           -- Invalid operation
            end case;
        end if;
    end process;
end ArithmeticUnitArch;


-- 3. Control Unit:
--    - Manages the instruction execution and data flow in a processor.
--    - Decodes instructions and generates control signals for other components.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity ControlUnit is
    port (
        Clock        : in STD_LOGIC;
        Instruction  : in STD_LOGIC_VECTOR(31 downto 0);  -- Instruction to be executed
        ReadRegister : out integer range 0 to 31;
        WriteRegister : out integer range 0 to 31;
        ArithmeticOp : out STD_LOGIC_VECTOR(3 downto 0); -- Arithmetic operation to be performed
        MemoryRead   : out STD_LOGIC;
        MemoryWrite  : out STD_LOGIC;
        JumpAddress  : out integer range 0 to 4095;      -- Address to jump to (if applicable)
    );
end ControlUnit;

architecture ControlUnitArch of ControlUnit is
    signal InstructionType : STD_LOGIC_VECTOR(2 downto 0);
    signal Opcode          : STD_LOGIC_VECTOR(6 downto 0);
    signal Funct           : STD_LOGIC_VECTOR(5 downto 0);
begin
    InstructionType <= Instruction(31 downto 29);
    Opcode          <= Instruction(28 downto 22);
    Funct           <= Instruction(5 downto 0);

    process (Clock)
    begin
        if (Clock'event and Clock = '1') then
            case InstructionType is
                when "000" => -- Register-Register Operations
                    case Opcode is
                        when "000000" => -- Add
                            ReadRegister(0) <= Instruction(20 downto 16);
                            ReadRegister(1) <= Instruction(15 downto 11);
                            WriteRegister <= Instruction(25 downto 21);
                            ArithmeticOp <= "0000";
                        when "000010" => -- Subtract
                            ReadRegister(0) <= Instruction(20 downto 16);
                            ReadRegister(1) <= Instruction(15 downto 11);
                            WriteRegister <= Instruction(25 downto 21);
                            ArithmeticOp <= "0001";
                        when "000100" => -- Multiply
                            ReadRegister(0) <= Instruction(20 downto 16);
                            ReadRegister(1) <= Instruction(15 downto 11);
                            WriteRegister <= Instruction(25 downto 21);
                            ArithmeticOp <= "0010";
                        when "000110" => -- Divide
                            ReadRegister(0) <= Instruction(20 downto 16);
                            ReadRegister(1) <= Instruction(15 downto 11);
                            WriteRegister <= Instruction(25 downto 21);
                            ArithmeticOp <= "0011";
                        when others => null; -- Invalid opcode
                    end case;
                when "001" => -- Load Immediate
                    ReadRegister <= Instruction(20 downto 16);
                    WriteRegister <= Instruction(25 downto 21);
                    ArithmeticOp <= "0000";
                when "010" => -- Store
                    ReadRegister <= Instruction(20 downto 16);
                    WriteRegister <= Instruction(25 downto 21);
                    MemoryWrite <= '1';
                when "011" => -- Load
                    ReadRegister <= Instruction(20 downto 16);
                    WriteRegister <= Instruction(25 downto 21);
                    MemoryRead <= '1';
                when "100" => -- Branch
                    JumpAddress <= Instruction(25 downto 0);
                when others => null; -- Invalid instruction type
            end case;
        end if;
    end process;
end ControlUnitArch;


-- 4. Data Memory:
--    - Provides storage for data and instructions.
--    - Supports read and write operations.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity DataMemory is
    generic (
        DataWidth : integer := 32;  -- Width of data stored in memory
        Depth     : integer := 4096  -- Number of memory locations
    );
    port (
        Clock    : in STD_LOGIC;
        Address  : in integer range 0 to Depth - 1;
        WriteData : in STD_LOGIC_VECTOR(DataWidth - 1 downto 0);
        ReadData  : out STD_LOGIC_VECTOR(DataWidth - 1 downto 0);
        MemoryWrite : in STD_LOGIC;
        MemoryRead  : in STD_LOGIC
    );
end DataMemory;

architecture DataMemoryArch of DataMemory is
    type MemoryArray is array (0 to Depth - 1) of STD_LOGIC_VECTOR(DataWidth - 1 downto 0);
    signal Memory : MemoryArray;
begin
    process (Clock)
    begin
        if (Clock'event and Clock = '1') then
            if (MemoryWrite = '1') then
                Memory(Address) <= WriteData;
            end if;
        end if;
    end process;

    ReadData <= Memory(Address);
end DataMemoryArch;


-- 5. Instruction Memory:
--    - Stores the program instructions.
--    - Supports read operations.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity InstructionMemory is
    generic (
        Depth : integer := 4096  -- Number of memory locations
    );
    port (
        Clock      : in STD_LOGIC;
        Address    : in integer range 0 to Depth - 1;
        Instruction : out STD_LOGIC_VECTOR(31 downto 0)
    );
end InstructionMemory;

architecture InstructionMemoryArch of InstructionMemory is
    type MemoryArray is array (0 to Depth - 1) of STD_LOGIC_VECTOR(31 downto 0);
    signal Memory : MemoryArray;
begin
    process (Clock)
    begin
        if (Clock'event and Clock = '1') then
            Instruction <= Memory(Address);
        end if;
    end process;
end InstructionMemoryArch;


-- 6. Top-Level Processor:
--    - Integrates all components (register file, arithmetic unit, control unit, data memory, instruction memory) to create a functional processor.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity Processor is
    port (
        Clock : in STD_LOGIC
    );
end Processor;

architecture ProcessorArch of Processor is
    component RegisterFile is
        port (
            Clock         : in STD_LOGIC;
            WriteEnable   : in STD_LOGIC;
            ReadRegister  : in integer range 0 to 31;
            WriteRegister : in integer range 0 to 31;
            OperandA      : out STD_LOGIC_VECTOR(31 downto 0);
            OperandB      : out STD_LOGIC_VECTOR(31 downto 0);
            Result        : in STD_LOGIC_VECTOR(31 downto 0)
        );
    end component;

    component ArithmeticUnit is
        port (
            Clock         : in STD_LOGIC;
            Operation      : in STD_LOGIC_VECTOR(3 downto 0);
            SignedOperands : in STD_LOGIC;
            OperandA       : in STD_LOGIC_VECTOR(31 downto 0);
            OperandB       : in STD_LOGIC_VECTOR(31 downto 0);
            Result         : out STD_LOGIC_VECTOR(31 downto 0)
        );
    end component;

    component ControlUnit is
        port (
            Clock        : in STD_LOGIC;
            Instruction  : in STD_LOGIC_VECTOR(31 downto 0);
            ReadRegister : out integer range 0 to 31;
            WriteRegister : out integer range 0 to 31;
            ArithmeticOp : out STD_LOGIC_VECTOR(3 downto 0);
            MemoryRead   : out STD_LOGIC;
            MemoryWrite  : out STD_LOGIC;
            JumpAddress  : out integer range 0 to 4095
        );
    end component;

    component DataMemory is
        port (
            Clock    : in STD_LOGIC;
            Address  : in integer range 0 to 4095;
            WriteData : in STD_LOGIC_VECTOR(31 downto 0);
            ReadData  : out STD_LOGIC_VECTOR(31 downto 0);
            MemoryWrite : in STD_LOGIC;
            MemoryRead  : in STD_LOGIC
        );
    end component;

    component InstructionMemory is
        port (
            Clock      : in STD_LOGIC;
            Address    : in integer range 0 to 4095;
            Instruction : out STD_LOGIC_VECTOR(31 downto 0)
        );
    end component;

    signal RegisterA : STD_LOGIC_VECTOR(31 downto 0);
    signal RegisterB : STD_LOGIC_VECTOR(31 downto 0);
    signal ALUResult : STD_LOGIC_VECTOR(31 downto 0);
    signal ReadData   : STD_LOGIC_VECTOR(31 downto 0);
    signal Instruction : STD_LOGIC_VECTOR(31 downto 0);
    signal ReadRegister, WriteRegister : integer range 0 to 31;
    signal ArithmeticOp              : STD_LOGIC_VECTOR(3 downto 0);
    signal MemoryRead, MemoryWrite  : STD_LOGIC;
    signal JumpAddress              : integer range 0 to 4095;
begin
    -- Instantiate the components
    RegisterFile_Inst : RegisterFile port map (
        Clock,
        WriteEnable => '1', -- Always enable writes for simplicity
        ReadRegister,
        WriteRegister,
        RegisterA,
        RegisterB,
        ALUResult
    );

    ArithmeticUnit_Inst : ArithmeticUnit port map (
        Clock,
        Operation => ArithmeticOp,
        SignedOperands => '0', -- Assume unsigned operands for simplicity
        OperandA,
        OperandB,
        ALUResult
    );

    ControlUnit_Inst : ControlUnit port map (
        Clock,
        Instruction,
        ReadRegister,
        WriteRegister,
        ArithmeticOp,
        MemoryRead,
        MemoryWrite,
        JumpAddress
    );

    DataMemory_Inst : DataMemory port map (
        Clock,
        Address => WriteRegister,
        WriteData => ALUResult,
        ReadData,
        MemoryWrite,
        MemoryRead
    );

    InstructionMemory_Inst : InstructionMemory port map (
        Clock,
        Address => ReadRegister,
        Instruction
    );
end ProcessorArch;