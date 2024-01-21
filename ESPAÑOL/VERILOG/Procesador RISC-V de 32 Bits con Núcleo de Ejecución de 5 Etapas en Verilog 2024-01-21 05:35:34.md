```verilog
// Este código Verilog implementa un procesador RISC-V de 32 bits con un núcleo de ejecución de 5 etapas.

// Definición de los registros especiales del procesador.
reg [31:0] pc;   // Contador de programa
reg [31:0] ir;    // Registro de instrucción
reg [31:0] reg_file[0:31];  // Archivo de registros
reg [31:0] alu_result;  // Resultado de la ALU
reg [31:0] mem_data;   // Datos leídos de la memoria

// Definición de las señales de control.
wire [1:0] alu_op;     // Operación de la ALU
wire [2:0] reg_dst;    // Destino del resultado de la ALU
wire [1:0] mem_op;     // Operación de memoria
wire        mem_write;  // Señal de escritura de memoria
wire        branch;     // Señal de salto condicional
wire        jump;       // Señal de salto incondicional

// Definición de la unidad de control.
always @(*) begin
  // Decodificación de la instrucción.
  case (ir[6:0])
    7'b0110011: begin  // Instrucción de adición.
      alu_op = 2'b00;   // Suma
      reg_dst = 3'b000;  // Destino: registro rs1
      mem_op = 2'b00;   // Ninguna operación de memoria
      mem_write = 1'b0;  // No escribir en memoria
      branch = 1'b0;    // No saltar condicionalmente
      jump = 1'b0;      // No saltar incondicionalmente
    end
    7'b0010011: begin  // Instrucción de sustracción.
      alu_op = 2'b01;   // Resta
      reg_dst = 3'b000;  // Destino: registro rs1
      mem_op = 2'b00;   // Ninguna operación de memoria
      mem_write = 1'b0;  // No escribir en memoria
      branch = 1'b0;    // No saltar condicionalmente
      jump = 1'b0;      // No saltar incondicionalmente
    end
    7'b0000011: begin  // Instrucción de carga.
      alu_op = 2'b00;   // Suma
      reg_dst = 3'b001;  // Destino: registro rd
      mem_op = 2'b10;   // Carga desde la memoria
      mem_write = 1'b0;  // No escribir en memoria
      branch = 1'b0;    // No saltar condicionalmente
      jump = 1'b0;      // No saltar incondicionalmente
    end
    7'b1010111: begin  // Instrucción de salto condicional.
      alu_op = 2'b00;   // Suma
      reg_dst = 3'bxxx;  // Destino: no importa
      mem_op = 2'b00;   // Ninguna operación de memoria
      mem_write = 1'b0;  // No escribir en memoria
      branch = 1'b1;    // Saltar condicionalmente
      jump = 1'b0;      // No saltar incondicionalmente
    end
    7'b1100111: begin  // Instrucción de salto incondicional.
      alu_op = 2'b00;   // Suma
      reg_dst = 3'bxxx;  // Destino: no importa
      mem_op = 2'b00;   // Ninguna operación de memoria
      mem_write = 1'b0;  // No escribir en memoria
      branch = 1'b0;    // No saltar condicionalmente
      jump = 1'b1;      // Saltar incondicionalmente
    end
    default: begin
      alu_op = 2'b00;   // Suma
      reg_dst = 3'bxxx;  // Destino: no importa
      mem_op = 2'b00;   // Ninguna operación de memoria
      mem_write = 1'b0;  // No escribir en memoria
      branch = 1'b0;    // No saltar condicionalmente
      jump = 1'b0;      // No saltar incondicionalmente
    end
  endcase
end

// Definición de la unidad aritmético-lógica (ALU).
always @(*) begin
  case (alu_op)
    2'b00: alu_result = reg_file[ir[19:15]] + reg_file[ir[24:20]];  // Suma
    2