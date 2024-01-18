```verilog
module TopLevelModule(
  input clk,
  input rst,
  input [7:0] data_in,
  output [7:0] data_out
);

  // Register to store the data
  reg [7:0] data_reg;

  // Finite state machine (FSM) to control the data processing
  reg [2:0] state;
  parameter IDLE = 0, SHIFT_IN = 1, SHIFT_OUT = 2;

  // Counter to keep track of the data shifting
  reg [3:0] counter;

  // Output register to store the processed data
  reg [7:0] data_out_reg;

  // FSM logic
  always @(posedge clk) begin
    if (rst) begin
      state <= IDLE;
      counter <= 0;
      data_out_reg <= 0;
    end else begin
      case (state)
        IDLE: begin
          if (data_in != 0) begin
            state <= SHIFT_IN;
            counter <= 0;
            data_reg <= data_in;
          end
        end
        SHIFT_IN: begin
          if (counter < 7) begin
            counter <= counter + 1;
            data_reg <= data_reg << 1;
          end else begin
            state <= SHIFT_OUT;
            counter <= 0;
          end
        end
        SHIFT_OUT: begin
          if (counter < 7) begin
            counter <= counter + 1;
            data_out_reg <= data_reg >> 1;
          end else begin
            state <= IDLE;
            counter <= 0;
          end
        end
      endcase
    end
  end

  // Output assignment
  assign data_out = data_out_reg;

endmodule
```

This Verilog code implements a complex data processing module with a finite state machine (FSM) and a counter. The module has three main operations:

1. **Shifting Data In:** When a non-zero value is received at the input `data_in`, the module enters the `SHIFT_IN` state. In this state, the input data is shifted into an internal register `data_reg` bit by bit until it reaches the 8th bit.

2. **Shifting Data Out:** After the data is fully shifted in, the module enters the `SHIFT_OUT` state. In this state, the data in `data_reg` is shifted out bit by bit and stored in the output register `data_out_reg`. This process continues until all 8 bits are shifted out.

3. **Idle State:** When no data is being shifted in or out, the module remains in the `IDLE` state.

The FSM controls the shifting process and ensures that the data is shifted in and out correctly. The counter keeps track of the current bit position during shifting. The output register `data_out_reg` stores the processed data, which is available at the output `data_out`.

This code performs a complex data shifting operation and demonstrates the use of FSMs and counters in Verilog design.