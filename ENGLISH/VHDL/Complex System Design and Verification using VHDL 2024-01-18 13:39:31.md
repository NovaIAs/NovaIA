```vhdl
--> Complex System Design Using VHDL <--

-- Define the top-level entity
entity System is
    -- Define the input and output ports
    Port (
        clk         : in std_logic;
        reset       : in std_logic;
        input_data  : in std_logic_vector(7 downto 0);
        output_data : out std_logic_vector(7 downto 0)
    );
end System;

-- Define the architecture of the system
architecture Behavioral of System is
    -- Internal signals
    signal reg_data  : std_logic_vector(7 downto 0);

begin
    -- Sequential process for data manipulation
    process(clk, reset)
    begin
        if reset = '1' then
            reg_data <= (others => '0');
        elsif rising_edge(clk) then
            reg_data <= input_data;
        end if;
    end process;

    -- Combinational logic for output
    output_data <= reg_data;
end Behavioral;

-- Define component for the Adder
component Adder is
    -- Define the input and output ports
    Port (
        a           : in std_logic_vector(7 downto 0);
        b           : in std_logic_vector(7 downto 0);
        sum         : out std_logic_vector(8 downto 0)
    );
end Adder;

-- Define the architecture of the Adder
architecture Structural of Adder is
begin
    -- Instantiate the full adder cells
    full_adder_0: FullAdder
        port map(
            a => a(0),
            b => b(0),
            cin => '0',
            sum => sum(0),
            cout => sum(1)
        );
    for i in 1 to 7 loop
        full_adder_i: FullAdder
            port map(
                a => a(i),
                b => b(i),
                cin => sum(i),
                sum => sum(i+1),
                cout => sum(i+2)
            );
    end loop;
end Structural;

-- Define component for the Full Adder
component FullAdder is
    -- Define the input and output ports
    Port (
        a           : in std_logic;
        b           : in std_logic;
        cin         : in std_logic;
        sum         : out std_logic;
        cout        : out std_logic
    );
end FullAdder;

-- Define the architecture of the Full Adder
architecture Behavioral of FullAdder is
begin
    -- Combinational logic for sum and carry
    sum <= a xor b xor cin;
    cout <= (a and b) or (a and cin) or (b and cin);
end Behavioral;

-- Testbench for the System
entity Testbench is
end Testbench;

architecture Behavioral of Testbench is
    -- Internal signals
    signal clk         : std_logic;
    signal reset       : std_logic;
    signal input_data  : std_logic_vector(7 downto 0);
    signal output_data : std_logic_vector(7 downto 0);

begin
    -- Clock generation
    clk <= not clk after 10 ns;

    -- Reset signal
    reset <= '1' after 10 ns, '0' after 20 ns;

    -- Input data
    input_data <= "00001010" after 30 ns, "10100101" after 40 ns;

    -- Instantiate the System component
    dut: System
        port map(
            clk         => clk,
            reset       => reset,
            input_data  => input_data,
            output_data => output_data
        );

    -- Monitor the output data
    process(clk)
    begin
        if rising_edge(clk) then
            report "Output data: " & std_logic_vector'image(output_data) & new_line;
        end if;
    end process;
end Behavioral;

**Explanation:**

- The top-level entity System defines the input and output ports of the system.
- The architecture Behavioral of System describes the behavior of the system using a sequential process and combinational logic.
- The Adder component is a separate module used for addition operations. Its architecture Structural defines the internal structure using FullAdder cells.
- The FullAdder component represents a basic building block for addition operations. Its architecture Behavioral defines the combinational logic for sum and carry.
- The Testbench entity and architecture create a test environment for verifying the system's functionality. It generates clock, reset, and input data signals and monitors the output data.

This code demonstrates a complex system design in VHDL, involving modularity, hierarchical design, and testbench for verification.