```vhdl
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity complex_vhdl_code is
    port (
        clk : in STD_LOGIC; -- Clock signal
        reset : in STD_LOGIC; -- Reset signal
        start : in STD_LOGIC; -- Start signal
        in_data : in STD_LOGIC_VECTOR(31 downto 0); -- Input data
        out_data : out STD_LOGIC_VECTOR(31 downto 0) -- Output data
    );
end complex_vhdl_code;

architecture behavioral of complex_vhdl_code is

    type state_type is (idle, processing, done);
    signal current_state, next_state : state_type := idle;

    -- Register to store the input data
    signal data_register : STD_LOGIC_VECTOR(31 downto 0);

    -- Flag to indicate when the processing is complete
    signal processing_complete : STD_LOGIC := '0';

    -- Temporary variable to store the intermediate result
    signal temp_result : STD_LOGIC_VECTOR(31 downto 0);

begin

    -- State machine process
    process(clk, reset)
    begin
        if reset = '1' then
            current_state <= idle;
        elsif rising_edge(clk) then
            current_state <= next_state;
        end if;
    end process;

    -- Next state logic
    next_state <= current_state;
    with current_state select
    when idle =>
        if start = '1' then
            next_state <= processing;
            data_register <= in_data;
        end if;
    when processing =>
        if processing_complete = '1' then
            next_state <= done;
        end if;
    when done =>
        next_state <= idle;
    end select;

    -- Processing logic
    process(clk)
    begin
        if rising_edge(clk) then
            if current_state = processing then
                -- Perform some complex calculations on the input data
                temp_result <= data_register;
                for i in 0 to 31 loop
                    temp_result(i) <= not temp_result(i);
                end loop;
                processing_complete <= '1';
            end if;
        end if;
    end process;

    -- Output logic
    out_data <= (others => '0');
    with current_state select
    when done =>
        out_data <= temp_result;
    end select;

end behavioral;
```

This VHDL code implements a complex computational task that involves inverting each bit of a 32-bit input value. The code uses a state machine to control the flow of operations and includes various logic blocks for data processing and output generation. Here's a brief explanation of how the code works:

1. **State Machine:** The code employs a three-state state machine: idle, processing, and done. It starts in the idle state, waiting for a start signal. When the start signal is asserted, the state machine transitions to the processing state. In the processing state, the code performs the necessary calculations, and when the processing is complete, it moves to the done state. Finally, it returns to the idle state when the done state is exited.

2. **Data Processing:** In the processing state, the code performs the core computational task: inverting each bit of the 32-bit input data. It uses a loop to iterate through each bit and flips its value from 0 to 1 or vice versa.

3. **Output Generation:** The output data is initially set to all zeros. When the state machine reaches the done state, it assigns the processed result (inverted input data) to the output data port.

Overall, this code demonstrates a complex combination of state machine control, data manipulation, and output generation, making it a comprehensive example of VHDL programming.