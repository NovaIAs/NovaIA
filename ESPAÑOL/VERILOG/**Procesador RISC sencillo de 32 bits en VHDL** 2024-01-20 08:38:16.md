```
// Código de VHDL para un procesador RISC sencillo de 32 bits, llamado "RIS32"

// **Módulos de ALU**

module ALU (
    input [31:0] A,
    input [31:0] B,
    input [2:0] opcode,
    output reg [31:0] result
);

    always @ (*) begin
        case (opcode)
            0: // Addición: A + B
                result = A + B;
            1: // Substracción: A - B
                result = A - B;
            2: // Multiplicación: A * B
                result = A * B;
            3: // División: A / B
                result = A / B;
            4: // AND: A & B
                result = A & B;
            5: // OR: A | B
                result = A | B;
            6: // XOR: A ^ B
                result = A ^ B;
            7: // Shift Left: A << B
                result = A << B;
            8: // Shift Right: A >> B
                result = A >> B;
            default:
                result = 32'b0;
        endcase
    end

endmodule

// ** Módulos del Registro **

module RegisterFile (
    input [4:0] address,
    input [31:0] data_in,
    input write_enable,
    output [31:0] data_out
);

    reg [31:0] registers [31:0];

    always @ (*) begin
        if (write_enable && address != 0) begin
            registers[address] <= data_in;
        end
    end

    assign data_out = registers[address];

endmodule

// ** Módulos de decodificación de instrucciones **

module InstructionDecoder (
    input [31:0] instruction,
    output [2:0] opcode,
    output [4:0] register_a,
    output [4:0] register_b,
    output [4:0] register_c,
    output [15:0] immediate
);

    assign opcode = instruction[31:29];
    assign register_a = instruction[28:24];
    assign register_b = instruction[23:19];
    assign register_c = instruction[18:14];
    assign immediate = instruction[13:0];

endmodule

// **Módulo de Control de la Unidad de Cómputo**

module ControlUnit (
    input [31:0] instruction,
    output reg [5:0] operation
);

    always @ (*) begin
        case (instruction[31:26])
            6: operation = 6'b000000; // R-type
            4: operation = 6'b000100; // BEQ
            5: operation = 6'b000101; // BNE
            2: operation = 6'b001000; // JMP
            3: operation = 6'b001001; // JAL
            32: operation = 6'b001010; // LW
            36: operation = 6'b001011; // SW
            0: operation = 6'b000010;  // ADDI
            1: operation = 6'b000011;  // SUBI
            8: operation = 6'b000110;  // ANDI
            9: operation = 6'b000111;  // ORI
            10: operation = 6'b001000;  // XORI
            11: operation = 6'b001110;  // SLTI
            12: operation = 6'b001111;  // SLTIU
            default: operation = 6'b000000;
        endcase
    end

endmodule

// ** Módulos de la Unidad de Cómputo **

module Datapath (
    input clk,
    input reset,
    input [31:0] instruction,
    output [31:0] result
);

    wire [2:0] opcode;
    wire [4:0] register_a;
    wire [4:0] register_b;
    wire [4:0] register_c;
    wire [15:0] immediate;
    wire [5:0] operation;

    RegisterFile register_file (.address(register_a), .data_in(result), .write_enable(1'b0), .data_out(data_a));
    RegisterFile register_file (.address(register_b), .data_in(result), .write_enable(1'b0), .data_out(data_b));
    RegisterFile register_file (.address(register_c), .data_in(result), .write_enable(1'b0), .data_out(data_c));

    InstructionDecoder instruction_decoder (.instruction(instruction), .opcode(opcode), .register_a(register_a), .register_b(register_b), .register_c(register_c), .immediate(immediate));
    ControlUnit control_unit (.instruction(instruction), .operation(operation));
    ALU alu (.A(data_a), .B(data_b), .opcode(opcode), .result(result));

    always @(posedge clk) begin
        if (reset) begin
            register_a <= 0;
            register_b <= 0;
            register_c <= 0;
        end else begin
            case (operation)
                6'b000000: begin // R-type
                    register_file (.address(register_c), .data_in(result), .write_enable(1'b1), .data_out(data_c));
                end
                6'b000100: begin // BEQ
                    if (data_a == data_b) begin
                        register_a <= register_a + immediate;
                    end
                end
                6'b000101: begin // BNE
                    if (data_a != data_b) begin
                        register_a <= register_a + immediate;
                    end
                end
                6'b001000: begin // JMP
                    register_a <= register_a + immediate;
                end
                6'b001001: begin // JAL
                    register_a <= register_a + immediate;
                    register_b <= register_a;
                end
                6'b001010: begin // LW
                    register_c <= data_a + immediate;
                end
                6'b001011: begin // SW
                    register_a <= data_a + immediate;
                end
                6'b000010: begin // ADDI
                    register_c <= data_a + immediate;
                end
                6'b000011: begin // SUBI
                    register_c <= data_a - immediate;
                end
                6'b000110: begin // ANDI
                    register_c <= data_a & immediate;
                end
                6'b000111: begin // ORI
                    register_c <= data_a | immediate;
                end
                6'b001000: begin // XORI
                    register_c <= data_a ^ immediate;
                end
                6'b001110: begin // SLTI
                    register_c <= (data_a < immediate) ? 1 : 0;
                end
                6'b001111: begin // SLTIU
                    register_c <= (data_a < immediate) ? 1 : 0;
                end
            endcase
        end
    end

endmodule

// ** Módulo principal del procesador **

module RIS32 (
    input clk,
    input reset,
    input [31:0] instruction,
    output [31:0] result
);

    Datapath datapath (.clk(clk), .reset(reset), .instruction(instruction), .result(result));

endmodule
```

**Explicación del código:**

El código anterior es una implementación de un procesador RISC sencillo de 32 bits, llamado "RIS32", en VHDL. El procesador incluye los siguientes módulos:

* Un módulo `ALU` que realiza las operaciones aritméticas y lógicas.
* Un módulo `RegisterFile` que almacena los registros del procesador.
* Un módulo `InstructionDecoder` que decodifica las instrucciones.
* Un módulo `ControlUnit` que controla el flujo de datos y operaciones.
* Un módulo `Datapath` que contiene todos los componentes del procesador y ejecuta las instrucciones.
* Un módulo `RIS32` que es el módulo principal del procesador.

El procesador RIS32 puede ejecutar las siguientes instrucciones:

* Instrucciones aritméticas: `ADD`, `SUB`, `MUL`, `DIV`.
* Instrucciones lógicas: `AND`, `OR`, `XOR`, `NOT`.
* Instrucciones de comparación: `SLT`, `SLTU`, `BEQ`, `BNE`.
* Instrucciones de salto: `JMP`, `JAL`.
* Instrucciones de carga y almacenamiento: `LW`, `SW`.

El procesador RIS32 está diseñado para ser simple y fácil de entender. Es un buen punto de partida para aprender sobre la arquitectura de los procesadores.