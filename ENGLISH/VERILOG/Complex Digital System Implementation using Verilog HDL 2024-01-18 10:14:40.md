```verilog
// Top-level module
module my_complex_system(
    input clk,
    input reset,
    input [7:0] data_in,
    output [15:0] data_out
);

// Register file
reg [15:0] registers[0:15];

// Adder
wire [15:0] adder_result;
assign adder_result = registers[0] + registers[1];

// Multiplier
wire [31:0] multiplier_result;
assign multiplier_result = registers[2] * registers[3];

// Comparator
wire comparator_result;
assign comparator_result = registers[4] > registers[5];

// Multiplexer
wire [15:0] multiplexer_result;
assign multiplexer_result = (comparator_result) ? registers[6] : registers[7];

// Shifter
wire [15:0] shifter_result;
assign shifter_result = registers[8] << 2;

// Decoder
wire [15:0] decoder_result;
assign decoder_result = (registers[9] == 0) ? 16'b0000000000000001 :
                        (registers[9] == 1) ? 16'b0000000000000010 :
                        (registers[9] == 2) ? 16'b0000000000000100 :
                        (registers[9] == 3) ? 16'b0000000000001000 :
                        (registers[9] == 4) ? 16'b0000000000010000 :
                        (registers[9] == 5) ? 16'b0000000000100000 :
                        (registers[9] == 6) ? 16'b0000000001000000 :
                        (registers[9] == 7) ? 16'b0000000010000000 :
                        (registers[9] == 8) ? 16'b0000000100000000 :
                        (registers[9] == 9) ? 16'b0000001000000000 :
                        (registers[9] == 10) ? 16'b0000010000000000 :
                        (registers[9] == 11) ? 16'b0000100000000000 :
                        (registers[9] == 12) ? 16'b0001000000000000 :
                        (registers[9] == 13) ? 16'b0010000000000000 :
                        (registers[9] == 14) ? 16'b0100000000000000 :
                        (registers[9] == 15) ? 16'b1000000000000000 : 16'b0;

// ROM
wire [15:0] rom_data;
rom #(
    .ADDRESS_WIDTH(4),
    .DATA_WIDTH(16),
    .INIT_FILE("rom_data.txt")
) rom_inst(
    .address(registers[10]),
    .data(rom_data)
);

// RAM
wire [15:0] ram_data;
ram #(
    .ADDRESS_WIDTH(8),
    .DATA_WIDTH(16),
    .INIT_FILE("ram_data.txt")
) ram_inst(
    .address(registers[11]),
    .data(ram_data),
    .write_enable(registers[12]),
    .read_write(registers[13])
);

// State machine
reg [2:0] state;
parameter STATE_IDLE = 0;
parameter STATE_LOAD_DATA = 1;
parameter STATE_PROCESS_DATA = 2;
parameter STATE_STORE_DATA = 3;

always @(posedge clk) begin
    if (reset) begin
        state <= STATE_IDLE;
        registers[0] <= 0;
        registers[1] <= 0;
        registers[2] <= 0;
        registers[3] <= 0;
        registers[4] <= 0;
        registers[5] <= 0;
        registers[6] <= 0;
        registers[7] <= 0;
        registers[8] <= 0;
        registers[9] <= 0;
        registers[10] <= 0;
        registers[11] <= 0;
        registers[12] <= 0;
        registers[13] <= 0;
        registers[14] <= 0;
        registers[15] <= 0;
    end else begin
        case (state)
            STATE_IDLE: begin
                if (data_in[7]) begin
                    state <= STATE_LOAD_DATA;
                end
            end
            STATE_LOAD_DATA: begin
                registers[data_in[3:0]] <= data_in[15:0];
                state <= STATE_PROCESS_DATA;
            end
            STATE_PROCESS_DATA: begin
                case (data_in[15:12])
                    0: registers[data_in[11:8]] <= adder_result;
                    1: registers[data_in[11:8]] <= multiplier_result;
                    2: registers[data_in[11:8]] <= multiplexer_result;
                    3: registers[data_in[11:8]] <= shifter_result;
                    4: registers[data_in[11:8]] <= decoder_result;
                    5: registers[data_in[11:8]] <= rom_data;
                    6: registers[data_in[11:8]] <= ram_data;
                    7: state <= STATE_STORE_DATA;
                    default: ;
                endcase
            end
            STATE_STORE_DATA: begin
                ram_inst.address(registers[14]);
                ram_inst.data(registers[15]);
                ram_inst.write_enable(1);
                ram_inst.read_write(1);
                state <= STATE_IDLE;
            end
        endcase
    end
end

// Output assignment
assign data_out = registers[15];

endmodule
```

This code implements a complex digital system using Verilog HDL. It includes various components like registers, adders, multipliers, comparators, multiplexers, shifters, decoders, ROM, RAM, and a state machine. The system performs various operations based on the input data and stores the results in registers or RAM. The detailed explanation of each component is as follows:

1. **Register File:** It consists of 16 16-bit registers, which can store intermediate values during the execution of operations.

2. **Adder:** It performs addition of two 16-bit values and stores the result in a register.

3. **Multiplier:** It multiplies two 16-bit values and stores the 32-bit result in a register.

4. **Comparator:** It compares two 16-bit values and generates a 1-bit output indicating if the first value is greater than the second value.

5. **Multiplexer:** It selects one of two 16-bit values based on a control signal and outputs the selected value.

6. **Shifter:** It shifts a 16-bit value left or right by a specified number of bits.

7. **Decoder:** It converts a 4-bit value into a 16-bit one-hot encoded value.

8. **ROM:** It is a read-only memory that stores data which can be accessed by an address.

9. **RAM:** It is a random-access memory that can be read from and written to using an address.

10. **State Machine:** It controls the flow of data and operations within the system. It can be in different states, such as IDLE, LOAD_DATA, PROCESS_DATA, and STORE_DATA, and transitions between states based on input signals.

11. **Output Assignment:** It connects the output of the system (the data stored in register 15) to the output port `data_out`.