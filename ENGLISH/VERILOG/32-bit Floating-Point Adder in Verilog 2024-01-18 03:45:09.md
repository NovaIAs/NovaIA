```verilog
// This code implements a 32-bit floating-point adder using Verilog.
// It uses the IEEE 754 standard for representing floating-point numbers.

module floating_point_adder(
    input [31:0] a,       // The first 32-bit floating-point number
    input [31:0] b,       // The second 32-bit floating-point number
    output [31:0] sum     // The 32-bit floating-point sum of a and b
);

    // Extract the sign, exponent, and mantissa from the inputs.
    wire sign_a = a[31];
    wire exponent_a = a[30:23];
    wire mantissa_a = a[22:0];

    wire sign_b = b[31];
    wire exponent_b = b[30:23];
    wire mantissa_b = b[22:0];

    // Determine the sign of the sum.
    wire sign_sum = sign_a ^ sign_b;

    // Determine the exponent of the sum.
    wire exponent_sum = exponent_a + exponent_b;

    // Add the mantissas.
    wire [23:0] mantissa_sum = mantissa_a + mantissa_b;

    // Normalize the mantissa.
    wire [23:0] normalized_mantissa_sum;
    wire exponent_sum_normalized;

    if (mantissa_sum[23]) begin
        normalized_mantissa_sum = mantissa_sum >> 1;
        exponent_sum_normalized = exponent_sum + 1;
    end
    else begin
        normalized_mantissa_sum = mantissa_sum;
        exponent_sum_normalized = exponent_sum;
    end

    // Pack the sign, exponent, and mantissa into the output.
    assign sum = {sign_sum, exponent_sum_normalized, normalized_mantissa_sum};

endmodule
```

This code implements a 32-bit floating-point adder using Verilog. It uses the IEEE 754 standard for representing floating-point numbers.

The code first extracts the sign, exponent, and mantissa from the inputs. The sign is a single bit that indicates whether the number is positive or negative. The exponent is an 8-bit field that represents the power of two by which the mantissa is multiplied. The mantissa is a 23-bit field that represents the fractional part of the number.

The code then determines the sign of the sum. If the signs of the two inputs are the same, the sign of the sum is the same as the signs of the inputs. If the signs of the two inputs are different, the sign of the sum is the opposite of the signs of the inputs.

The code then determines the exponent of the sum. The exponent of the sum is the sum of the exponents of the two inputs.

The code then adds the mantissas. The mantissas are added using a 23-bit adder.

The code then normalizes the mantissa. The mantissa is normalized by shifting it right until the most significant bit is 1. The exponent is incremented by 1 for each shift.

The code then packs the sign, exponent, and mantissa into the output. The output is a 32-bit floating-point number.