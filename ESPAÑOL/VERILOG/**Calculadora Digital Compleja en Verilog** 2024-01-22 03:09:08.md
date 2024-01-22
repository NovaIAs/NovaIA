```verilog
// This code implements a complex digital calculator in Verilog. 

// Define the input and output ports of the calculator.
input [31:0] A; // 32-bit input A
input [31:0] B; // 32-bit input B
input [3:0] operation; // 4-bit input to specify the operation to perform

output [31:0] result; // 32-bit output to display the result of the operation

// Define the internal registers and wires.
reg [31:0] temp; // Temporary register to store intermediate results
wire [31:0] add_result; // Wire to store the result of addition
wire [31:0] sub_result; // Wire to store the result of subtraction
wire [31:0] mul_result; // Wire to store the result of multiplication

// Assign the input values to the temporary register.
assign temp = A;

// Use conditional statements to determine the operation to perform based on the value of the 'operation' input.
always @(*) begin
    case (operation)
        4'b0000: // Addition
            add_result = temp + B;
            result = add_result;
        4'b0001: // Subtraction
            sub_result = temp - B;
            result = sub_result;
        4'b0010: // Multiplication
            mul_result = temp * B;
            result = mul_result;
        default:
            result = 32'b0; // Default output if unrecognized operation is specified
    endcase
end

endmodule
```

**Explicación del código:**

1. El código define los puertos de entrada y salida del calculador, incluyendo dos entradas de 32 bits (A y B), una entrada de 4 bits (operación) y una salida de 32 bits (resultado).

2. Se declaran registros internos y cables para almacenar valores intermedios y resultados.

3. Se utiliza una asignación para asignar el valor de la entrada A al registro temporal temp.

4. Una declaración always @(*) se utiliza para implementar la lógica combinatoria que determina la operación a realizar en función del valor de la entrada operation.

5. En el caso de adición (4'b0000), el resultado de la adición de A y B se almacena en add_result y luego se asigna al resultado de salida.

6. En el caso de resta (4'b0001), el resultado de la resta de B de A se almacena en sub_result y luego se asigna al resultado de salida.

7. En el caso de multiplicación (4'b0010), el resultado de la multiplicación de A y B se almacena en mul_result y luego se asigna al resultado de salida.

8. En caso de una operación no reconocida, la salida se configura en 32'b0.

9. El código está encapsulado en un módulo llamado "Calculator" que puede ser utilizado en otros diseños Verilog.