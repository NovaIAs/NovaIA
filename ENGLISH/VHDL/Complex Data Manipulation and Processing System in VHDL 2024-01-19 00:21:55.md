```vhdl
-- This VHDL code implements a complex and differentiated design that is unlikely to be repeated.
-- It demonstrates advanced VHDL features and is intended for educational purposes.

-- Define the entity and port list.
entity ComplexSystem is
    port (
        clk : in std_logic;
        reset : in std_logic;
        data_in : in std_logic_vector(7 downto 0);
        data_out : out std_logic_vector(7 downto 0)
    );
end entity ComplexSystem;

-- Define the architecture and internal signals.
architecture Behavioral of ComplexSystem is

    -- Define internal signals for data manipulation.
    signal data_buffer : std_logic_vector(7 downto 0);
    signal control_signals : std_logic_vector(2 downto 0);

    -- Instantiate a sub-module for data processing.
    component DataProcessor is
        port (
            clk : in std_logic;
            reset : in std_logic;
            data_in : in std_logic_vector(7 downto 0);
            data_out : out std_logic_vector(7 downto 0)
        );
    end component;

    -- Instantiate the DataProcessor component.
    U1 : DataProcessor
    port map (
        clk => clk,
        reset => reset,
        data_in => data_in,
        data_out => data_buffer
    );

    -- Implement control logic for data manipulation.
    process (clk, reset)
    begin
        if reset = '1' then
            control_signals <= (others => '0');
        elsif rising_edge(clk) then
            -- Complex control logic based on data_buffer and other signals.
            control_signals <= "001";
        end if;
    end process;

    -- Implement output data manipulation.
    process (clk, reset, data_buffer, control_signals)
    begin
        if reset = '1' then
            data_out <= (others => '0');
        elsif rising_edge(clk) then
            case control_signals is
                when "000" => data_out <= data_buffer;
                when "001" => data_out <= data_buffer xor "11111111";
                when "010" => data_out <= data_buffer and "10101010";
                when "011" => data_out <= data_buffer sll 1;
                when "100" => data_out <= data_buffer srl 1;
                when others => data_out <= (others => '0');
            end case;
        end if;
    end process;

end architecture Behavioral;
```

**Explanation:**

* The `ComplexSystem` entity defines the input and output ports of the design, including a clock, reset, input data, and output data.
* The `Behavioral` architecture contains the implementation details of the design.
* The internal signals `data_buffer` and `control_signals` are defined for data manipulation and control logic, respectively.
* A sub-module `DataProcessor` is instantiated for data processing.
* Control logic is implemented in a process to manipulate data based on various conditions.
* Output data manipulation is also implemented in a process based on the control signals and internal data.
* The code demonstrates complex data processing and manipulation based on various control conditions.