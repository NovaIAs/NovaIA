```verilog
// Top-level module: instantiate and interconnect submodules
module TopModule(input wire clk,  // Clock input
                   input wire rst,  // Reset input
                   output wire [3:0] an,  // 7-segment display anode outputs
                   output wire [6:0] seg); // 7-segment display segment outputs

    // Instantiate submodules
    wire [3:0] BCD;  // 4-bit binary-coded decimal digits
    wire [6:0] seg_raw;  // Raw 7-segment display segments

    BCDCounter BCDCounterInst(clk, rst, BCD);  // Instantiate BCD counter
    SevenSegmentDecoder SevenSegmentDecoderInst(BCD, seg_raw);  // Instantiate 7-segment decoder

    // Assign segment outputs
    assign seg = {seg_raw[6], seg_raw[4], seg_raw[3], seg_raw[2], seg_raw[1], seg_raw[0], seg_raw[5]};

    // Assign anode outputs
    assign an = {1'b1, 1'b1, 1'b1, 1'b1};  // All anodes are active (common-anode display)

endmodule


// BCD counter: counts from 0 to 9 in binary-coded decimal
module BCDCounter(input wire clk,  // Clock input
                    input wire rst,  // Reset input
                    output reg [3:0] BCD);  // 4-bit binary-coded decimal digits

    // State machine states
    localparam STATE_0 = 4'b0000;
    localparam STATE_1 = 4'b0001;
    localparam STATE_2 = 4'b0010;
    localparam STATE_3 = 4'b0011;
    localparam STATE_4 = 4'b0100;
    localparam STATE_5 = 4'b0101;
    localparam STATE_6 = 4'b0110;
    localparam STATE_7 = 4'b0111;
    localparam STATE_8 = 4'b1000;
    localparam STATE_9 = 4'b1001;

    // State register and next state logic
    reg [3:0] state, next_state;
    always @(posedge clk) begin
        if (rst) begin
            state <= STATE_0;
        end else begin
            state <= next_state;
        end
    end

    always @(*) begin
        case (state)
            STATE_0: next_state = STATE_1;
            STATE_1: next_state = STATE_2;
            STATE_2: next_state = STATE_3;
            STATE_3: next_state = STATE_4;
            STATE_4: next_state = STATE_5;
            STATE_5: next_state = STATE_6;
            STATE_6: next_state = STATE_7;
            STATE_7: next_state = STATE_8;
            STATE_8: next_state = STATE_9;
            STATE_9: next_state = STATE_0;
        endcase
    end

    // Output logic
    always @(*) begin
        case (state)
            STATE_0: BCD = 4'b0000;
            STATE_1: BCD = 4'b0001;
            STATE_2: BCD = 4'b0010;
            STATE_3: BCD = 4'b0011;
            STATE_4: BCD = 4'b0100;
            STATE_5: BCD = 4'b0101;
            STATE_6: BCD = 4'b0110;
            STATE_7: BCD = 4'b0111;
            STATE_8: BCD = 4'b1000;
            STATE_9: BCD = 4'b1001;
        endcase
    end

endmodule


// 7-segment decoder: converts 4-bit BCD digits to 7-segment display segments
module SevenSegmentDecoder(input wire [3:0] BCD,  // 4-bit binary-coded decimal digits
                             output reg [6:0] seg); // 7-segment display segments

    // Segment patterns for each BCD digit
    localparam [6:0] SEG_0 = 7'b1111110;
    localparam [6:0] SEG_1 = 7'b0110000;
    localparam [6:0] SEG_2 = 7'b1101101;
    localparam [6:0] SEG_3 = 7'b1111001;
    localparam [6:0] SEG_4 = 7'b0110011;
    localparam [6:0] SEG_5 = 7'b1011011;
    localparam [6:0] SEG_6 = 7'b1011111;
    localparam [6:0] SEG_7 = 7'b1110000;
    localparam [6:0] SEG_8 = 7'b1111111;
    localparam [6:0] SEG_9 = 7'b1111011;

    // Output logic
    always @(*) begin
        case (BCD)
            4'b0000: seg = SEG_0;
            4'b0001: seg = SEG_1;
            4'b0010: seg = SEG_2;
            4'b0011: seg = SEG_3;
            4'b0100: seg = SEG