module Shift_Register_with_Reset (
    input clock,
    input reset,
    input [7:0] data_in,
    output reg [7:0] data_out
);

// Register to hold the data
reg [7:0] data_register;

// Reset logic
always @(posedge reset or posedge clock) begin
    if (reset) begin
        data_register <= 8'b0;
    end else begin
        data_register <= {data_register[6:0], data_in};
    end
end

// Output logic
always @(*) begin
    data_out <= data_register;
end

endmodule

This code implements a shift register with a reset function. The idea behind a shift register is to store multiple bits of data in a sequential manner, and the data is shifted by one position on each clock cycle. This module also includes a reset function that allows the data in the register to be cleared to all zeros.

The code has four major blocks:

1. Input Ports:
   - clock: This is the clock signal that drives the shifting operation.
   - reset: This is the reset signal that clears the data in the register.
   - data_in: This is the 8-bit data that is to be shifted into the register.

2. Output Ports:
   - data_out: This is the 8-bit data that is stored in the register and is shifted out on each clock cycle.

3. Register Logic:
   - data_register: This is an 8-bit register that holds the data being shifted.

4. Reset Logic:
   - This logic is implemented using an "always" block that is triggered on both the positive edge of the reset signal and the positive edge of the clock signal. When the reset signal is asserted (high), the data register is cleared to all zeros. Otherwise, the data is shifted by one position, with the new data bit being shifted in from the data_in port.

This module can be used to store and shift data in digital circuits, making it a useful building block for various digital systems.