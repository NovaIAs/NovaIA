```vhdl
-- This complex VHDL code demonstrates various advanced concepts and provides a detailed explanation of each section.

-- 1. Entity Declaration:
entity ComplexArithmetic is
    -- Define input and output ports.
    port (
        clk : in std_logic; -- Clock input
        rst : in std_logic; -- Reset input
        a : in std_logic_vector(31 downto 0); -- 32-bit signed input A
        b : in std_logic_vector(31 downto 0); -- 32-bit signed input B
        op : in std_logic_vector(1 downto 0); -- 2-bit operation selector
        result : out std_logic_vector(31 downto 0) -- 32-bit signed result
    );
end ComplexArithmetic;

-- 2. Architecture Behavioral Description:
architecture Behavioral of ComplexArithmetic is
    -- Define internal signals.
    signal op_add, op_sub, op_mul, op_div : std_logic; -- Operation control signals
    signal temp : std_logic_vector(31 downto 0); -- Temporary storage for intermediate results

    -- Decode operation selector.
    op_add <= op(1) and not op(0);
    op_sub <= op(1) and op(0);
    op_mul <= not op(1) and op(0);
    op_div <= not op(1) and not op(0);

    -- Perform selected operation.
    with select op_add select
        temp <= a + b when '00',
        temp <= a - b when '01',
        temp <= a * b when '10',
        temp <= a / b when '11';

    -- Handle division by zero.
    result <= temp when not op_div or b /= (others => '0'),
             others => "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"; -- Set result to all 'Z's if division by zero

end Behavioral;
```

**Explanation:**

1. **Entity Declaration**:
   - The `entity` declaration defines the interface of the VHDL component, including the input and output ports.

2. **Architecture Behavioral Description**:
   - The `architecture` section contains the behavioral description of the component.
   - The `with select op_add select` statement uses a case statement to select the operation to be performed based on the value of the `op` input.
   - The `temp` signal is used to store intermediate results.
   - The `result` output is assigned the value of `temp` when the operation is not division or when the divisor is not zero. Otherwise, it is set to all 'Z's to indicate an invalid result.