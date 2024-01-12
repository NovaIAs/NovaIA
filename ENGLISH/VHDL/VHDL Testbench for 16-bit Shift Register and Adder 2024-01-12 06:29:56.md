```vhdl
-- Define the top-level entity
entity top_entity is
    -- Define the input and output ports
    port (
        clk : in std_logic;
        reset : in std_logic;
        data_in : in std_logic_vector(7 downto 0);
        data_out : out std_logic_vector(15 downto 0)
    );
end entity;

-- Define the architecture for the top-level entity
architecture behavioral of top_entity is
    -- Define the internal signals
    signal shift_register : std_logic_vector(15 downto 0) := (others => '0');

begin
    -- Shift register logic
    shift_register <= shift_register(14 downto 0) & data_in;

    -- Output logic
    data_out <= shift_register;
end architecture;

-- Define a component for a 4-bit adder
component adder is
    -- Define the input and output ports
    port (
        a : in std_logic_vector(3 downto 0);
        b : in std_logic_vector(3 downto 0);
        sum : out std_logic_vector(4 downto 0);
        carry : out std_logic
    );
end component;

-- Define the architecture for the adder component
architecture structural of adder is
    -- Instantiate the carry-lookahead adder
    signal carry_in : std_logic := '0';
    signal carry_out : std_logic;
    signal sum : std_logic_vector(4 downto 0);

begin
    carry_lookahead_adder: carry_lookahead_adder_4bit
    port map (
        a => a,
        b => b,
        sum => sum,
        carry_in => carry_in,
        carry_out => carry_out
    );
end architecture;

-- Define a component for a 16-bit adder
component adder_16bit is
    -- Define the input and output ports
    port (
        a : in std_logic_vector(15 downto 0);
        b : in std_logic_vector(15 downto 0);
        sum : out std_logic_vector(16 downto 0);
        carry : out std_logic
    );
end component;

-- Define the architecture for the 16-bit adder component
architecture structural of adder_16bit is
    -- Instantiate four 4-bit adders
    signal a_low : std_logic_vector(3 downto 0);
    signal a_high : std_logic_vector(3 downto 0);
    signal b_low : std_logic_vector(3 downto 0);
    signal b_high : std_logic_vector(3 downto 0);
    signal sum_low : std_logic_vector(4 downto 0);
    signal sum_high : std_logic_vector(4 downto 0);
    signal carry : std_logic;

begin
    -- Split the inputs into four 4-bit segments
    a_low <= a(3 downto 0);
    a_high <= a(7 downto 4);
    b_low <= b(3 downto 0);
    b_high <= b(7 downto 4);

    -- Instantiate the four 4-bit adders
    adder_0: adder
    port map (
        a => a_low,
        b => b_low,
        sum => sum_low,
        carry => carry
    );

    adder_1: adder
    port map (
        a => a_high,
        b => b_high,
        sum => sum_high,
        carry => carry
    );

    -- Combine the four 4-bit segments into a 16-bit sum
    sum <= sum_low & sum_high;
end architecture;

-- Define a testbench for the top-level entity
entity top_entity_tb is
end entity;

architecture behavioral of top_entity_tb is
    -- Define the test signals
    signal clk : std_logic := '0';
    signal reset : std_logic := '1';
    signal data_in : std_logic_vector(7 downto 0) := (others => '0');
    signal data_out : std_logic_vector(15 downto 0);

begin
    -- Clock generator
    clk_gen: process
    begin
        while true loop
            clk <= '1';
            wait for 10 ns;
            clk <= '0';
            wait for 10 ns;
        end loop;
    end process;

    -- Reset generator
    reset_gen: process
    begin
        reset <= '0';
        wait for 100 ns;
        reset <= '1';
    end process;

    -- Data input generator
    data_in_gen: process
    begin
        for i in 0 to 255 loop
            data_in <= std_logic_vector(to_unsigned(i, 8));
            wait for 10 ns;
        end loop;
    end process;

    -- Instantiate the top-level entity
    uut: top_entity
    port map (
        clk => clk,
        reset => reset,
        data_in => data_in,
        data_out => data_out
    );

    -- Monitor the output signals
    output_monitor: process
    begin
        while true loop
            wait until rising_edge(clk);
            if (reset = '0') then
                report "Reset active, output: " & std_logic_vector'image(data_out);
            else
                report "Output: " & std_logic_vector'image(data_out);
            end if;
        end loop;
    end process;
end architecture;
```

This code is a testbench for a top-level entity that contains a 16-bit shift register and a 16-bit adder. The testbench generates clock, reset, and data input signals, and monitors the output signals of the top-level entity. The code is written in VHDL, a hardware description language used for designing digital systems.