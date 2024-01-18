```vhdl
-- This VHDL code implements a complex and differentiated circuit,
-- which includes various features and components.
-- It is designed to showcase a wide range of VHDL capabilities.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity Complex_Circuit is
    port (
        clk : in STD_LOGIC;
        reset : in STD_LOGIC;
        data_in : in STD_LOGIC_VECTOR(7 downto 0);
        data_out : out STD_LOGIC_VECTOR(15 downto 0)
    );
end Complex_Circuit;

architecture Behavioral of Complex_Circuit is

    -- Define internal signals and components
    signal reg_a : STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
    signal reg_b : STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
    signal reg_c : STD_LOGIC_VECTOR(15 downto 0) := (others => '0');
    component Adder is
        port (
            a : in STD_LOGIC_VECTOR(7 downto 0);
            b : in STD_LOGIC_VECTOR(7 downto 0);
            sum : out STD_LOGIC_VECTOR(8 downto 0)
        );
    end component;
    component Comparator is
        port (
            a : in STD_LOGIC_VECTOR(7 downto 0);
            b : in STD_LOGIC_VECTOR(7 downto 0);
            equal : out STD_LOGIC
        );
    end component;
    component Register is
        port (
            clk : in STD_LOGIC;
            reset : in STD_LOGIC;
            data : in STD_LOGIC_VECTOR(7 downto 0);
            q : out STD_LOGIC_VECTOR(7 downto 0)
        );
    end component;

begin

    -- Instantiate components
    adder1 : Adder port map (a => reg_a, b => reg_b, sum => reg_c);
    comparator1 : Comparator port map (a => reg_a, b => reg_b, equal => equal_signal);

    -- Register for data_in
    reg_data_in : Register port map (clk => clk, reset => reset, data => data_in, q => reg_a);

    -- Register for reg_b
    reg_reg_b : Register port map (clk => clk, reset => reset, data => data_in, q => reg_b);

    -- Combinational logic to calculate data_out
    data_out <= reg_c when equal_signal else (others => '0');

end Behavioral;
```

**Explanation:**

1. **Component Declarations:**
    - Adder: This component implements an 8-bit adder with inputs `a` and `b`, and an output `sum`.
    - Comparator: This component checks for equality between two 8-bit vectors `a` and `b`, and outputs a `equal` signal indicating the result.
    - Register: This component acts as a D-type register with a clock, reset, data input, and a registered output.

2. **Signal Declarations:**
    - `reg_a` and `reg_b`: These are 8-bit registers used to store data.
    - `reg_c`: This is a 16-bit register used to store the output of the adder.
    - `equal_signal`: This is a 1-bit signal that indicates whether the contents of `reg_a` and `reg_b` are equal.

3. **Component Instantiations:**
    - `adder1`: An instance of the Adder component is connected to the `reg_a`, `reg_b`, and `reg_c` signals.
    - `comparator1`: An instance of the Comparator component is connected to the `reg_a` and `reg_b` signals, and it outputs the `equal_signal`.

4. **Register Instantiation:**
    - `reg_data_in`: A register is connected to the `data_in` input and updates the value of `reg_a` based on the clock and reset signals.
    - `reg_reg_b`: Another register is also connected to the `data_in` input and updates the value of `reg_b` based on the clock and reset signals.

5. **Output Logic:**
    - `data_out`: The output is determined based on the `equal_signal`. If the contents of `reg_a` and `reg_b` are equal, the output is set to the contents of `reg_c`. Otherwise, the output is set to all zeros.

This code implements a circuit that checks for equality between two 8-bit values (`data_in`) and stores the result in `data_out`. When the values are equal, the sum of the two values is stored in `data_out`. When the values are different, `data_out` is set to zero.