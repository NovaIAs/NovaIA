```verilog
// 1. Definiciones y librerías
`timescale 1ns / 1ps
`include "defines.v"

// 2. Módulos
// 2.1 Módulo CPU
module cpu (
    input clk,
    input reset,
    // Buses
    output [31:0] addr, // Dirección de memoria
    input [31:0] data_in, // Dato de entrada desde la memoria
    output [31:0] data_out, // Dato de salida a la memoria
    output [3:0] write_enable // Habilitación de escritura en memoria
);

// 2.1.1 Registro de instrucciones (IR)
reg [31:0] ir;

// 2.1.2 Registro de programa (PC)
reg [31:0] pc;

// 2.1.3 Registro de banderas (FLAGS)
reg [31:0] flags;

// 2.1.4 Registro de propósito general (GPR)
reg [31:0] gpr [0:31];

// 2.1.5 Unidad aritmético-lógica (ALU)
wire [31:0] alu_result;
wire [31:0] alu_flags;

// 2.1.6 Unidad de control (CU)
wire [31:0] next_pc;
wire [4:0] opcode;
wire [5:0] funct;

// 2.1.7 Unidad de datos (DU)
wire [31:0] read_data1;
wire [31:0] read_data2;
wire [31:0] write_data;
wire [4:0] write_reg;

// 2.1.8 Conexión entre módulos
assign opcode = ir[31:26];
assign funct = ir[5:0];
assign write_reg = ir[20:16];

// 2.2 Módulo ALU
module alu (
    input [31:0] a,
    input [31:0] b,
    input [2:0] op,
    output [31:0] result,
    output [31:0] flags
);

// 2.2.1 Codificación de operaciones
localparam [2:0] OP_ADD = 3'b000;
localparam [2:0] OP_SUB = 3'b001;
localparam [2:0] OP_AND = 3'b010;
localparam [2:0] OP_OR = 3'b011;
localparam [2:0] OP_XOR = 3'b100;
localparam [2:0] OP_SLT = 3'b101;
localparam [2:0] OP_SGT = 3'b110;
localparam [2:0] OP_SEQ = 3'b111;

// 2.2.2 Lógica
always @(*) begin
    case (op)
        OP_ADD: begin
            result = a + b;
            flags = {31'b0, (result[31] != a[31] && result[31] != b[31])};
        end
        OP_SUB: begin
            result = a - b;
            flags = {31'b0, (result[31] != a[31] && result[31] == b[31])};
        end
        OP_AND: begin
            result = a & b;
            flags = 32'b0;
        end
        OP_OR: begin
            result = a | b;
            flags = 32'b0;
        end
        OP_XOR: begin
            result = a ^ b;
            flags = 32'b0;
        end
        OP_SLT: begin
            result = (a < b) ? 32'b1 : 32'b0;
            flags = 32'b0;
        end
        OP_SGT: begin
            result = (a > b) ? 32'b1 : 32'b0;
            flags = 32'b0;
        end
        OP_SEQ: begin
            result = (a == b) ? 32'b1 : 32'b0;
            flags = 32'b0;
        end
        default: begin
            result = 32'bx;
            flags = 32'bx;
        end
    endcase
end

// 2.3 Módulo CU
module cu (
    input [31:0] ir,
    output [31:0] next_pc,
    output [4:0] opcode,
    output [5:0] funct
);

// 2.3.1 Codificación de instrucciones
localparam [4:0] I_ADD = 5'b00000;
localparam [4:0] I_SUB = 5'b00001;
localparam [4:0] I_AND = 5'b00010;
localparam [4:0] I_OR = 5'b00011;
localparam [4:0] I_XOR = 5'b00100;
localparam [4:0] I_SLT = 5'b00101;
localparam [4:0] I_SGT = 5'b00110;
localparam [4:0] I_SEQ = 5'b00111;
localparam [4:0] I_LW = 5'b01000;
localparam [4:0] I_SW = 5'b01001;
localparam [4:0] I_BEQ = 5'b01010;
localparam [4:0] I_BNE = 5'b01011;
localparam [4:0] I_J = 5'b01100;
localparam [4:0] I_JAL = 5'b01101;

// 2.3.2 Lógica
always @(*) begin
    next_pc = pc + 4;

    case (ir[31:26])
        I_ADD: opcode = 5'b00000;
        I_SUB: opcode = 5'b00001;
        I_AND: opcode = 5'b00010;
        I_OR: opcode = 5'b00011;
        I_XOR: opcode = 5'b00100;
        I_SLT: opcode = 5'b00101;
        I_SGT: opcode = 5'b00110;
        I_SEQ: opcode = 5'b00111;
        I_LW: opcode = 5'b01000;
        I_SW: opcode = 5'b01001;
        I_BEQ: opcode = 5'b01010;
        I_BNE: opcode = 5'b01011;
        I_J: opcode = 5'b01100;
        I_JAL: opcode = 5'b01101;
        default: opcode = 5'bx;
    endcase

    funct = ir[5:0];
end

// 2.4 Módulo DU
module du (
    input [31:0] pc,
    input [31:0] ir,
    input [31:0] read_data1,
    input [31:0] read_data2,
    output [31:0] write_data,
    output [4:0] write_reg
);

// 2.4.1 Lógica
always @(*) begin
    write_data = read_data2;
    write_reg = 0;

    case (ir[31:26])
        I_LW: begin
            write_reg = ir[20:16];
            write_data = data_in;
        end
        I_SW: begin
            write_data = read_data1;
        end
        default: ;
    endcase
end

// 2.5 Módulo CPU (continuación)
// 2.5.1 Registro de instrucción (IR)
always @(posedge clk) begin
    if (reset) begin
        ir <= 32'b0;
    end else begin
        ir <= data_in;
    end
end

// 2.5.2 Registro de programa (PC)
always @(posedge clk) begin
    if (reset) begin
        pc <= 32'h0;
    end else begin
        pc <= next_pc;
    end
end

// 2.5.3 Registro de banderas (FLAGS)
always @(posedge clk) begin
    if (reset) begin
        flags <= 32'b0;
    end else begin
        flags <= alu_flags;
    end
end

// 2.5.4 Registro de propósito general (GPR)
always @(posedge clk) begin
    if (reset) begin
        for (int i = 0; i < 32; ++i) begin
            gpr[i] <= 32'b0;
        end
    end else begin
        if (write_reg != 0) begin
            gpr[write_reg] <= write_data;
        end
    end
end

// 2.5.5 Unidad aritmético-lógica (ALU)
assign alu_result = alu(read_data1, read_data2, opcode, alu_result, alu_flags);

// 2.5.6 Unidad de control (CU)
assign next_pc = cu(ir, next_pc, opcode, funct);

// 2.5.7 Unidad de datos (DU)
assign {read_data1, read_data2} = {gpr[ir[25:21]], gpr[ir[20:16]]};

endmodule