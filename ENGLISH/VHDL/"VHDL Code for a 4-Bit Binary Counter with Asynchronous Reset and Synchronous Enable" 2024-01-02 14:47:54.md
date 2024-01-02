Certainly! Here's a complex and unique VHDL code that implements a 4-bit binary counter with an asynchronous reset and a synchronous enable signal. Let me explain each part of the code for you:

```vhdl
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Binary_Counter is
    port (
        clock     : in  std_logic;
        reset     : in  std_logic;
        enable    : in  std_logic;
        count_out : out std_logic_vector(3 downto 0)
    );
end entity Binary_Counter;

architecture Behavioral of Binary_Counter is
    signal counter : unsigned(3 downto 0);
begin
    process (clock, reset)
    begin
        if reset = '1' then
            counter <= (others => '0');
        elsif rising_edge(clock) then
            if enable = '1' then
                counter <= counter + 1;
            end if;
        end if;
    end process;

    count_out <= std_logic_vector(counter);
end architecture Behavioral;
```

This VHDL code describes a 4-bit binary counter with the ability to asynchronously reset the counter to zero and to enable/disable counting based on an input signal.

The `entity` section defines the input and output ports of the counter. It has four ports: `clock`, `reset`, `enable`, and `count_out`. The `clock` input is used as the clock signal, `reset` input is used to reset the counter asynchronously, `enable` input is used to enable or disable counting, and `count_out` output provides the current count value in binary representation.

The `architecture` section is where the behavior of the counter is defined. It includes a process block sensitive to changes in `clock` and `reset` signals. 

Inside the process, when `reset` is asserted, the counter is asynchronously reset to zero. This is done by assigning the counter signal with all zeros.

On the rising edge of the `clock` signal, if the `enable` signal is asserted, the counter is incremented by 1 using the `counter <= counter + 1` statement. This increment is performed using the `unsigned` type from the `ieee.numeric_std` library, which allows arithmetic operations on signals.

Finally, `count_out` is assigned the value of `counter` converted to a `std_logic_vector` type, so that it can be used as an output.

This code provides a highly configurable and reusable 4-bit binary counter that can be easily integrated into larger digital designs.