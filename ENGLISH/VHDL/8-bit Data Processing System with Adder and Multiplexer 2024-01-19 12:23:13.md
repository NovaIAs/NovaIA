```vhdl
-- Top-level module
entity Top is
    Port (
        clk : in STD_LOGIC;
        reset : in STD_LOGIC;
        data_in : in STD_LOGIC_VECTOR(7 downto 0);
        data_out : out STD_LOGIC_VECTOR(7 downto 0)
    );
end entity Top;

-- Architecture for the top-level module
architecture Behavioral of Top is

    -- Instantiate the data processing unit
    component DataProcessingUnit is
        Port (
            clk : in STD_LOGIC;
            reset : in STD_LOGIC;
            data_in : in STD_LOGIC_VECTOR(7 downto 0);
            data_out : out STD_LOGIC_VECTOR(7 downto 0)
        );
    end component DataProcessingUnit;

    -- Create an instance of the data processing unit
    signal data_processed : STD_LOGIC_VECTOR(7 downto 0);
    U1 : DataProcessingUnit
    port map (
        clk => clk,
        reset => reset,
        data_in => data_in,
        data_out => data_processed
    );

    -- Instantiate the output register
    component OutputRegister is
        Port (
            clk : in STD_LOGIC;
            reset : in STD_LOGIC;
            data_in : in STD_LOGIC_VECTOR(7 downto 0);
            data_out : out STD_LOGIC_VECTOR(7 downto 0)
        );
    end component OutputRegister;

    -- Create an instance of the output register
    signal data_out_reg : STD_LOGIC_VECTOR(7 downto 0);
    U2 : OutputRegister
    port map (
        clk => clk,
        reset => reset,
        data_in => data_processed,
        data_out => data_out_reg
    );

    -- Assign the output of the output register to the output port
    data_out <= data_out_reg;

end architecture Behavioral;

-- Data processing unit component
component DataProcessingUnit is
    Port (
        clk : in STD_LOGIC;
        reset : in STD_LOGIC;
        data_in : in STD_LOGIC_VECTOR(7 downto 0);
        data_out : out STD_LOGIC_VECTOR(7 downto 0)
    );
end component DataProcessingUnit;

-- Architecture for the data processing unit
architecture Behavioral of DataProcessingUnit is

    -- Instantiate the adder
    component Adder is
        Port (
            a : in STD_LOGIC_VECTOR(7 downto 0);
            b : in STD_LOGIC_VECTOR(7 downto 0);
            sum : out STD_LOGIC_VECTOR(8 downto 0)
        );
    end component Adder;

    -- Create an instance of the adder
    signal sum : STD_LOGIC_VECTOR(8 downto 0);
    U1 : Adder
    port map (
        a => data_in,
        b => data_in,
        sum => sum
    );

    -- Instantiate the multiplexer
    component Multiplexer is
        Port (
            sel : in STD_LOGIC;
            a : in STD_LOGIC_VECTOR(7 downto 0);
            b : in STD_LOGIC_VECTOR(7 downto 0);
            out : out STD_LOGIC_VECTOR(7 downto 0)
        );
    end component Multiplexer;

    -- Create an instance of the multiplexer
    signal data_out_mux : STD_LOGIC_VECTOR(7 downto 0);
    U2 : Multiplexer
    port map (
        sel => reset,
        a => sum(7 downto 0),
        b => data_in,
        out => data_out_mux
    );

    -- Assign the output of the multiplexer to the output port
    data_out <= data_out_mux;

end architecture Behavioral;

-- Adder component
component Adder is
    Port (
        a : in STD_LOGIC_VECTOR(7 downto 0);
        b : in STD_LOGIC_VECTOR(7 downto 0);
        sum : out STD_LOGIC_VECTOR(8 dow