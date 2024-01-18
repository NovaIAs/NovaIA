```vhdl
-- This VHDL code implements a complex and differentiated design, consisting of a memory controller, a DMA controller, and a processor core. It is not likely to be repeated again.

-- Memory Controller
entity Memory_Controller is
    Port (
        CLOCK    : in STD_LOGIC;
        RESET    : in STD_LOGIC;
        ADDRESS  : in STD_LOGIC_VECTOR(31 downto 0);
        DATA_IN  : in STD_LOGIC_VECTOR(31 downto 0);
        DATA_OUT : out STD_LOGIC_VECTOR(31 downto 0);
        RW       : in STD_LOGIC  -- Read/Write control signal
    );
end Memory_Controller;

architecture Behavioral of Memory_Controller is
    type Memory is array (0 to 4095) of STD_LOGIC_VECTOR(31 downto 0);

    signal Memory_Data : Memory;
    signal Read_Address : STD_LOGIC_VECTOR(11 downto 0);
    signal Write_Address : STD_LOGIC_VECTOR(11 downto 0);
    signal Write_Data : STD_LOGIC_VECTOR(31 downto 0);
    signal Read_Data : STD_LOGIC_VECTOR(31 downto 0);
    signal RW_Control : STD_LOGIC;

begin
    -- Memory Initialization
    for i in 0 to 4095 loop
        Memory_Data(i) := (others => '0');
    end loop;

    -- Read Address Register
    Read_Address_Register: process(CLOCK, RESET)
    begin
        if RESET = '1' then
            Read_Address <= (others => '0');
        elsif rising_edge(CLOCK) then
            Read_Address <= ADDRESS(11 downto 0);
        end if;
    end process;

    -- Write Address Register
    Write_Address_Register: process(CLOCK, RESET)
    begin
        if RESET = '1' then
            Write_Address <= (others => '0');
        elsif rising_edge(CLOCK) then
            Write_Address <= ADDRESS(11 downto 0);
        end if;
    end process;

    -- Write Data Register
    Write_Data_Register: process(CLOCK, RESET)
    begin
        if RESET = '1' then
            Write_Data <= (others => '0');
        elsif rising_edge(CLOCK) then
            Write_Data <= DATA_IN;
        end if;
    end process;

    -- Read Data Register
    Read_Data_Register: process(CLOCK, RESET)
    begin
        if RESET = '1' then
            Read_Data <= (others => '0');
        elsif rising_edge(CLOCK) then
            Read_Data <= Memory_Data(Read_Address);
        end if;
    end process;

    -- RW Control Register
    RW_Control_Register: process(CLOCK, RESET)
    begin
        if RESET = '1' then
            RW_Control <= '0';
        elsif rising_edge(CLOCK) then
            RW_Control <= RW;
        end if;
    end process;

    -- Memory Read/Write
    Memory_Read_Write: process(CLOCK, RESET)
    begin
        if RESET = '1' then
            for i in 0 to 4095 loop
                Memory_Data(i) <= (others => '0');
            end loop;
        elsif rising_edge(CLOCK) then
            if RW_Control = '0' then -- Read operation
                DATA_OUT <= Read_Data;
            elsif RW_Control = '1' then -- Write operation
                Memory_Data(Write_Address) <= Write_Data;
            end if;
        end if;
    end process;
end Behavioral;

-- DMA Controller
entity DMA_Controller is
    Port (
        CLOCK       : in STD_LOGIC;
        RESET       : in STD_LOGIC;
        DMA_START   : in STD_LOGIC;
        DMA_ADDRESS : in STD_LOGIC_VECTOR(31 downto 0);
        DMA_LENGTH  : in STD_LOGIC_VECTOR(31 downto 0);
        DMA_DATA    : in STD_LOGIC_VECTOR(31 downto 0);
        DATA_OUT    : out STD_LOGIC_VECTOR(31 downto 0)
    );
end DMA_Controller;

architecture Behavioral of DMA_Controller is
    signal DMA_Active : STD_LOGIC;
    signal DMA_Counter : STD_LOGIC_VECTOR(31 downto 0);
    signal DMA_Address : STD_LOGIC_VECTOR(31 downto 0);

begin
    -- DMA Active Register
    DMA_Active_Register: process(CLOCK, RESET)
    begin
        if RESET = '1' then
            DMA_Active <= '0';
        elsif rising_edge(CLOCK) then
            DMA_Active <= DMA_START;
        end if;
    end process;

    -- DMA Counter Register
    DMA_Counter_Register: process(CLOCK, RESET)
    begin
        if RESET = '1' then
            DMA_Counter <= (others => '0');
        elsif rising_edge(CLOCK) and DMA_Active = '1' then
            if DMA_Counter = DMA_LENGTH then
                DMA_Counter <= (others => '0');
            else
                DMA_Counter <= DMA_Counter + 1;
            end if;
        end if;
    end process;

    -- DMA Address Register
    DMA_Address_Register: process(CLOCK, RESET)
    begin
        if RESET = '1' then
            DMA_Address <= (others => '0');
        elsif rising_edge(CLOCK) and DMA_Active = '1' then
            DMA_Address <= DMA_ADDRESS;
        end if;
    end process;

    -- DMA Data Output
    DATA_OUT <= Memory_Controller.DATA_OUT;
end Behavioral;

-- Processor Core
entity Processor_Core is
    Port (
        CLOCK    : in STD_LOGIC;
        RESET    : in STD_LOGIC;
        ADDRESS  : in STD_LOGIC_VECTOR(31 downto 0);
        DATA_IN  : in STD_LOGIC_VECTOR(31 downto 0);
        DATA