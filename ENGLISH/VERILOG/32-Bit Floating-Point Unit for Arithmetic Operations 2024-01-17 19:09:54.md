```verilog
// This Verilog code implements a 32-bit Floating-Point Unit (FPU) capable of performing various arithmetic operations.

module FloatingPointUnit (
    // Input ports
    clk,
    reset,
    operation,
    a,
    b,

    // Output ports
    result,
    status
);

// Clock and reset signals
input clk;
input reset;

// Operation to be performed
input [3:0] operation;

// 32-bit floating-point inputs
input [31:0] a;
input [31:0] b;

// 32-bit floating-point result
output [31:0] result;

// Status flags indicating the result of the operation
output [3:0] status;

// Internal registers
reg [31:0] mantissa_a;
reg [31:0] exponent_a;
reg [31:0] mantissa_b;
reg [31:0] exponent_b;
reg [31:0] result_mantissa;
reg [31:0] result_exponent;
reg [3:0] result_status;

// Instantiate the submodules
FPAdder adder(.clk(clk), .reset(reset), .a_mantissa(mantissa_a), .a_exponent(exponent_a), .b_mantissa(mantissa_b), .b_exponent(exponent_b), .result_mantissa(result_mantissa), .result_exponent(result_exponent), .result_status(result_status));
FPMultiplier multiplier(.clk(clk), .reset(reset), .a_mantissa(mantissa_a), .a_exponent(exponent_a), .b_mantissa(mantissa_b), .b_exponent(exponent_b), .result_mantissa(result_mantissa), .result_exponent(result_exponent), .result_status(result_status));
FPDivider divider(.clk(clk), .reset(reset), .a_mantissa(mantissa_a), .a_exponent(exponent_a), .b_mantissa(mantissa_b), .b_exponent(exponent_b), .result_mantissa(result_mantissa), .result_exponent(result_exponent), .result_status(result_status));

// Logic for operation selection
always @ (posedge clk) begin
    if (reset) begin
        mantissa_a <= 0;
        exponent_a <= 0;
        mantissa_b <= 0;
        exponent_b <= 0;
        result_mantissa <= 0;
        result_exponent <= 0;
        result_status <= 0;
    end else begin
        case (operation)
            ADD: begin
                mantissa_a <= a[30:0];
                exponent_a <= a[31:23];
                mantissa_b <= b[30:0];
                exponent_b <= b[31:23];
                adder.perform_add <= 1'b1;
                multiplier.perform_multiply <= 1'b0;
                divider.perform_divide <= 1'b0;
            end
            SUB: begin
                mantissa_a <= a[30:0];
                exponent_a <= a[31:23];
                mantissa_b <= b[30:0];
                exponent_b <= b[31:23];
                adder.perform_add <= 1'b0;
                multiplier.perform_multiply <= 1'b0;
                divider.perform_divide <= 1'b0;
            end
            MUL: begin
                mantissa_a <= a[30:0];
                exponent_a <= a[31:23];
                mantissa_b <= b[30:0];
                exponent_b <= b[31:23];
                adder.perform_add <= 1'b0;
                multiplier.perform_multiply <= 1'b1;
                divider.perform_divide <= 1'b0;
            end
            DIV: begin
                mantissa_a <= a[30:0];
