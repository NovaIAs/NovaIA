```vhdl
-- 8-bit Comparator with Overflow Detection

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Comparator is
    port (
        a, b : in std_logic_vector(7 downto 0);
        eq, gt, lt, ovf : out std_logic
    );
end entity;

architecture Behavioral of Comparator is
    signal a_int, b_int : integer range -128 to 127;
begin
    a_int <= signed(a);
    b_int <= signed(b);

    eq <= a_int = b_int;
    gt <= a_int > b_int;
    lt <= a_int < b_int;

    -- Overflow detection
    ovf <= (a_int > 0 and b_int > 0 and a_int + b_int < 0) or
           (a_int < 0 and b_int < 0 and a_int + b_int > 0);
end architecture;

-- Testbench for Comparator

library ieee;
use ieee.std_logic_1164.all;

entity Comparator_Testbench is
end entity;

architecture Behavioral of Comparator_Testbench is
    constant A_VALUES : std_logic_vector(7 downto 0) array (1 to 16) := (
        "00000000", "00000001", "00000010", "00000011",
        "00000100", "00000101", "00000110", "00000111",
        "00001000", "00001001", "00001010", "00001011",
        "00001100", "00001101", "00001110", "00001111"
    );

    constant B_VALUES : std_logic_vector(7 downto 0) array (1 to 16) := (
        "00000000", "00000001", "00000010", "00000011",
        "00000100", "00000101", "00000110", "00000111",
        "00001000", "00001001", "00001010", "00001011",
        "00001100", "00001101", "00001110", "00001111"
    );

    signal a, b : std_logic_vector(7 downto 0);
    signal eq, gt, lt, ovf : std_logic;

begin
    Comparator_Instance : Comparator port map (
        a => a,
        b => b,
        eq => eq,
        gt => gt,
        lt => lt,
        ovf => ovf
    );

    process
    begin
        for i in 1 to A_VALUES'length loop
            a <= A_VALUES(i);
            for j in 1 to B_VALUES'length loop
                b <= B_VALUES(j);

                report "A = " & std_logic_vector'image(a) & ", B = " &
                       std_logic_vector'image(b) & ", EQ = " & std_logic'image(eq) &
                       ", GT = " & std_logic'image(gt) & ", LT = " &
                       std_logic'image(lt) & ", OV = " & std_logic'image(ovf);
            end loop;
        end loop;

        wait;
    end process;
end architecture;
```

Explanation:

This VHDL code implements an 8-bit comparator with overflow detection. It compares two 8-bit signed integers (a and b) and outputs the following information:

* `eq`: True if a and b are equal, False otherwise.
* `gt`: True if a is greater than b, False otherwise.
* `lt`: True if a is less than b, False otherwise.
* `ovf`: True if there is an overflow in the comparison, False otherwise.

The comparator is implemented using two integer signals, `a_int` and `b_int`, which are used to store the signed integer values of a and b, respectively. The comparison operations are then performed on these integer signals.

The overflow detection logic uses the fact that an overflow occurs when the sum of two positive integers becomes negative, or the sum of two negative integers becomes positive. This is detected by checking the signs of a_int and b_int, and the sign of their sum.

The testbench for the comparator generates all possible combinations of 8-bit signed integer values for a and b, and displays the results of the comparison. This allows us to verify the correctness of the comparator's implementation.