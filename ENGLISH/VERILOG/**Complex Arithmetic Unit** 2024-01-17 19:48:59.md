**Module Name:** `ComplexArithmeticUnit`

**Description:**

This module implements a complex arithmetic unit that can perform addition, subtraction, multiplication, and division of complex numbers. The complex numbers are represented as 32-bit floating-point numbers, with the real and imaginary parts stored in separate registers.

**Inputs:**

* `clk`: The clock input.
* `rst`: The reset input.
* `a_real`, `a_imag`: The real and imaginary parts of the first complex number.
* `b_real`, `b_imag`: The real and imaginary parts of the second complex number.
* `op`: The operation to be performed. This can be one of the following values:
  * `0`: Addition
  * `1`: Subtraction
  * `2`: Multiplication
  * `3`: Division

**Outputs:**

* `result_real`, `result_imag`: The real and imaginary parts of the result of the operation.

**Internal Registers:**

* `a_reg_real`, `a_reg_imag`: Registers to store the real and imaginary parts of the first complex number.
* `b_reg_real`, `b_reg_imag`: Registers to store the real and imaginary parts of the second complex number.
* `result_reg_real`, `result_reg_imag`: Registers to store the real and imaginary parts of the result of the operation.

**Operations:**

The module performs the following operations, depending on the value of the `op` input:

* Addition: `result = a + b`
* Subtraction: `result = a - b`
* Multiplication: `result = a * b`
* Division: `result = a / b`

**Implementation:**

The module is implemented using a combination of combinational logic and registers. The combinational logic performs the arithmetic operations, and the registers store the intermediate and final results.

**Testbench:**

A testbench is provided to test the module. The testbench generates random complex numbers and performs all four operations on them. The results are checked against the expected values.

**Usage:**

This module can be used in a variety of applications, such as digital signal processing, image processing, and scientific computing.

**Code:**

```verilog
// Complex Arithmetic Unit

module ComplexArithmeticUnit(
  input clk,
  input rst,
  input [31:0] a_real,
  input [31:0] a_imag,
  input [31:0] b_real,
  input [31:0] b_imag,
  input [1:0] op,
  output [31:0] result_real,
  output [31:0] result_imag
);

  // Registers to store the inputs
  reg [31:0] a_reg_real;
  reg [31:0] a_reg_imag;
  reg [31:0] b_reg_real;
  reg [31:0] b_reg_imag;

  // Registers to store the result
  reg [31:0] result_reg_real;
  reg [31:0] result_reg_imag;

  // Combinational logic to perform the operations
  wire [31:0] add_real = a_reg_real + b_reg_real;
  wire [31:0] add_imag = a_reg_imag + b_reg_imag;
  wire [31:0] sub_real = a_reg_real - b_reg_real;
  wire [31:0] sub_imag = a_reg_imag - b_reg_imag;
  wire [63:0] mul_real_imag = (a_reg_real * b_reg_real) - (a_reg_imag * b_reg_imag);
  wire [63:0] mul_imag_real = (a_reg_imag * b_reg_real) + (a_reg_real * b_reg_imag);
  wire [31:0] div_real = (a_reg_real * b_reg_real + a_reg_imag * b_reg_imag) / (b_reg_real * b_reg_real + b_reg_imag * b_reg_imag);
  wire [31:0] div_imag = (b_reg_real * a_reg_imag - a_reg_real * b_reg_imag) / (b_reg_real * b_reg_real + b_reg_imag * b_reg_imag);

  // Select the result based on the operation
  always @(*) begin
    case (op)
      0: begin
        result_reg_real = add_real;
        result_reg_imag = add_imag;
      end
      1: begin
        result_reg_real = sub_real;
        result_reg_imag = sub_imag;
      end
      2: begin
        result_reg_real = mul_real_imag[31:0];
        result_reg_imag = mul_imag_real[31:0];
      end
      3: begin
        result_reg_real =