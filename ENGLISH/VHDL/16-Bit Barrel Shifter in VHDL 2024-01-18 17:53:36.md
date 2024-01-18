```vhdl
-- Entity declaration for a 16-bit Barrel Shifter
entity barrel_shifter is
    generic (
        DATA_WIDTH : integer := 16 -- Width of the data input
    );
    port (
        data_in     : in std_logic_vector(DATA_WIDTH-1 downto 0);
        shift_amount : in integer range 0 to DATA_WIDTH-1;
        shift_dir   : in std_logic; -- '0' for left shift, '1' for right shift
        data_out    : out std_logic_vector(DATA_WIDTH-1 downto 0)
    );
end entity barrel_shifter;

-- Architecture declaration for the 16-bit Barrel Shifter
architecture Behavioral of barrel_shifter is
    -- Define the shift registers for temporary storage of data
    signal shift_registers : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');

begin

    -- Multiplexer for selecting the appropriate input to the shift registers
    for i in 0 to DATA_WIDTH-1 generate
        shift_registers(i) <=
            mux(
                shift_dir,           -- Select line
                data_in(i),          -- Data input
                shift_registers(i-1) -- Previous shift register output
            );
    end generate;

    -- Assignment of shifted data to the output
    data_out <= shift_registers(DATA_WIDTH-1-shift_amount);

end architecture Behavioral;

-- Auxiliary function for multiplexing
function mux(sel : std_logic; a, b : std_logic) return std_logic is
begin
    if sel = '0' then
        return a;
    else
        return b;
    end if;
end function mux;
```

Explanation:

This VHDL code implements a 16-bit barrel shifter. It takes a 16-bit input data, a shift amount, and a shift direction as inputs, and produces the shifted data as the output.

The code starts with the entity declaration, which defines the interface of the barrel shifter. It specifies the generic parameter DATA_WIDTH, which determines the width of the data input and output, and the ports data_in, shift_amount, shift_dir, and data_out.

Next, the architecture declaration defines the internal structure of the barrel shifter. It uses a shift register approach to implement the shifting operation. An array of shift registers, shift_registers, is defined to temporarily store the data during the shifting process.

The core of the code is a loop that uses a multiplexer to select the appropriate input for each shift register. The multiplexer takes the shift direction, the current data input, and the output of the previous shift register as inputs, and selects the input based on the shift direction.

Finally, the shifted data is assigned to the output port, data_out, by selecting the appropriate element from the shift_registers array.

Overall, this VHDL code provides a flexible and efficient implementation of a 16-bit barrel shifter, making it suitable for various applications that require shifting operations.