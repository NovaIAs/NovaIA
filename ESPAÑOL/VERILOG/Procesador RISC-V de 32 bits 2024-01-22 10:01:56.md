```verilog
// Módulo principal del diseño de un procesador RISC-V de 32 bits.

module procesador_RISC_V(
    input  wire        clk,      // Reloj principal del procesador.
    input  wire        rst,      // Señal de reset del procesador.
    input  wire [31:0] data_in,  // Entrada de datos para el procesador.
    output wire [31:0] data_out, // Salida de datos del procesador.
    output wire        data_out_valid, // Señal de validez de la salida de datos.
    output wire        data_in_ready  // Señal de disponibilidad de la entrada de datos.
);

// Constantes utilizadas en el diseño.

// Tamaño de la palabra en bits.
parameter word_size = 32;

// Número de registros en el banco de registros.
parameter num_registers = 32;

// Códigos de operación de las instrucciones RISC-V.
parameter opcode_add = 6'b000000;
parameter opcode_sub = 6'b000010;
parameter opcode_and = 6'b000100;
parameter opcode_or = 6'b000110;
parameter opcode_xor = 6'b001000;
parameter opcode_jalr = 6'b110011;
parameter opcode_lw = 6'b000001;
parameter opcode_sw = 6'b010001;
parameter opcode_beq = 6'b110000;
parameter opcode_bne = 6'b110001;
parameter opcode_blt = 6'b110100;
parameter opcode_bge = 6'b110101;

// Registro de instrucciones (IR).
reg [31:0] ir;

// Registro de datos (DR).
reg [31:0] dr;

// Contador de programa (PC).
reg [31:0] pc;

// Banco de registros (RF).
reg [31:0] rf[num_registers-1:0];

// Unidad de control (CU).
wire [4:0] opcode;
wire [2:0] funct3;
wire [5:0] funct7;
wire [4:0] rs1;
wire [4:0] rs2;
wire [4:0] rd;
wire jalr;
wire jal;
wire beq;
wire bne;
wire blt;
wire bge;

// Unidad aritmético-lógica (ALU).
wire [31:0] alu_out;
wire zero;
wire carry;
wire overflow;

// Unidad de carga/almacenamiento de memoria (LSU).
wire [31:0] mem_addr;
wire [31:0] mem_data;
wire mem_write;
wire mem_read;

// Unidad de salto (JU).
wire [31:0] pc_next;

// Decodificación de la instrucción en curso.
decode_instruction(ir, opcode, funct3, funct7, rs1, rs2, rd, jalr, jal, beq, bne, blt, bge);

// Ejecución de la instrucción en curso.
execute_instruction(opcode, funct3, funct7, rs1, rs2, rd, jalr, jal, beq, bne, blt, bge, alu_out, zero, carry, overflow, mem_addr, mem_data, mem_write, mem_read);

// Actualización del registro de datos (DR).
assign data_out = dr;
assign data_out_valid = 1;
assign data_in_ready  = 1;

// Actualización del registro de instrucciones (IR).
always @(posedge clk) begin
    if (rst) begin
        ir <= 0;
    end else begin
        ir <= data_in;
    end
end

// Actualización del contador de programa (PC).
always @(posedge clk) begin
    if (rst) begin
        pc <= 0;
    end else begin
        pc <= pc_next;
    end
end

// Actualización del banco de registros (RF).
always @(posedge clk) begin
    if (rst) begin
        for (integer i = 0; i < num_registers; i = i + 1) begin
            rf[i] <= 0;
        end
    end else begin
        if (mem_write) begin
            rf[rd] <= mem_data;
        end else if (jalr) begin
            rf[rd] <= pc_next;
        end else if (jal) begin
            rf[rd] <= pc + 4;
        end else if (rd != 0) begin
            rf[rd] <= alu_out;
        end
    end
end

// Instanciación de la unidad de control (CU).
control_unit cu(
    .opcode(opcode),
    .funct3(funct3),
    .funct7(funct7),
    .jalr(jalr),
    .jal(jal),
    .beq(beq),
    .bne(bne),
    .blt(blt),
    .bge(bge)
);

// Instanciación de la unidad aritmético-lógica (ALU).
alu alu(
    .a(rf[rs1]),
    .b(rf[rs2]),
    .opcode(opcode),
    .funct3(funct3),
    .funct7(funct7),
    .alu_out(alu_out),
    .zero(zero),
    .carry(carry),
    .overflow(overflow)
);

// Instanciación de la unidad de carga/almacenamiento de memoria (LSU).
lsu lsu(
    .clk(clk),
    .addr(mem_addr),
    .data_in(rf[rs2]),
    .data_out(mem_data),
    .write(mem_write),
    .read(mem_read)
);

// Instanciación de la unidad de salto (JU).
jump_unit ju(
    .pc(pc),
    .offset(alu_out),
    .jalr(jalr),
    .jal(jal),
    .beq(beq),
    .bne(bne),
    .blt(blt),
    .bge(bge),
    .zero(zero),
    .pc_next(pc_next)
);

endmodule

// Bloque de decodificación de la instrucción en curso.

module decode_instruction(
    input  wire [31:0] ir,
    output wire [4:0] opcode,
    output wire [2:0] funct3,
    output wire [5:0] funct7,
    output wire [4:0] rs1,
    output wire [4:0] rs2,
    output wire [4:0] rd,
    output wire        jalr,
    output wire        jal,
    output wire        beq,
    output wire        bne,
    output wire        blt,
    output wire        bge
);

assign opcode = ir[6:2];
assign funct3 = ir[14:12];
assign funct7 = ir[31:25];
assign rs1 = ir[19:15];
assign rs2 = ir[24:20];
assign rd = ir[11:7];
assign jalr = (opcode == opcode_jalr);
assign jal = (opcode == opcode_jal);
assign beq = (opcode == opcode_beq);
assign bne = (opcode == opcode_bne);
assign blt = (opcode == opcode_blt);
assign bge = (opcode == opcode_bge);

endmodule

// Bloque de ejecución de la instrucción en curso.

module execute_instruction(
    input  wire [4:0] opcode,
    input  wire [2:0] funct3,
    input  wire [5:0] funct7,
    input  wire [4:0] rs1,
    input  wire [4:0] rs2,
    input  wire [4:0] rd,
    input  wire        jalr,
    input  wire        jal,
    input  wire        beq,
    input  wire        bne,
    input  wire        blt,
    input  wire        bge,
    output wire [31:0] alu_out,
    output wire        zero,
    output wire        carry,
    output wire        overflow,
    output wire [31:0] mem_addr,
    output wire [31:0] mem_data,
    output wire        mem_write,
    output wire        mem_read
);

wire [31:0] rs1_data;
wire [31:0] rs2_data;
wire [31:0] alu_in2;
wire [31:0] pc_offset;

assign rs1_data = rf[rs1];
assign rs2_data = rf[rs2];

// Cálculo de la dirección de memoria para las instrucciones de carga/almacenamiento.
assign mem_addr = rs1_data + alu_in2;

// Selección de la entrada de la ALU en función del código de operación.
mux2_32bit alu_in2_mux(
    .sel(jalr),
    .in1(pc + 4),
    .in2(rs2_data),
    .out(alu_in2)
);

// Cálculo del desplazamiento de programa para las instrucciones de salto.
assign pc_offset = {{11{alu_in2[31]}}, alu_in2[30:0]};

// Selección de la instrucción a ejecutar en función del código de operación.
mux8_32bit alu_out_mux(
    .sel(opcode),
    .in1(rs1_data + alu_in2),             // add
    .in2(rs1_data - alu_in2),             // sub
    .in3(rs1_data & alu_in2),             // and
    .in4(rs1_data | alu_in2),             // or
    .in5(rs1_data ^ alu_in2),             // xor
    .in6(alu_in2 << funct3),             // sll
    .in7(alu_in2 >> funct3),             // srl
    .in8(alu_in2 >>> funct3),            // sra
    .out(alu_out)
);

// Generación de las señales de control de memoria para las instrucciones de carga/almacenamiento.
assign mem_write = (opcode == opcode_sw);
assign mem_read = ((opcode == opcode_lb) || (opcode == opcode_lh) || (opcode == opcode_lw));

// Generación de los registros de salida en función del código de operación.
mux2_32bit dr_mux(
    .sel(jalr || jal),
    .in1(pc_offset),
    .in2(alu_out),
    .out(dr)
);

endmodule

// Bloque de control de la unidad aritmético-lógica (ALU).

module control_unit(
    input  wire [4:0] opcode,
    input  wire [2:0] funct3,
    input  wire [5:0] funct7,
    output wire        jalr,
    output wire        jal,
    output wire        beq,
    output wire        bne,
    output wire        blt,
    output wire        bge
);

assign jalr = (opcode == opcode_jalr);
assign jal = (opcode == opcode_jal);
assign beq = (opcode == opcode_beq);
assign bne = (opcode == opcode_bne);
assign blt = (opcode == opcode_blt);
assign bge = (opcode == opcode_bge);

endmodule

// Bloque de selección de 2 entradas de 32 bits.

module mux2_32bit(
    input  wire        sel,
    input  wire [31:0] in1,
    input  wire [31:0] in2,
    output wire [31:0] out
);

assign out = (sel) ? in2 : in1;

endmodule

// Bloque de selección de 8 entradas de 32 bits.

module mux8_32bit(
    input  wire [2:0] sel,
    input  wire [31:0] in1,
    input  wire [31:0] in2,
    input  wire [31:0] in3,
    input  wire [31:0] in4,
    input  wire [31:0] in5,
    input  wire [31:0] in6,
    input  wire [31:0] in7,
    input  wire [31:0] in8,
    output wire [31:0] out
);

assign out = (sel == 0) ? in1 :
             (sel == 1) ? in2 :
             (sel == 2) ? in3 :
             (sel == 3) ? in4 :
             (sel == 4) ? in5 :
             (sel == 5) ? in6 :
             (sel == 6) ? in7 : in8;

endmodule`enter code here`