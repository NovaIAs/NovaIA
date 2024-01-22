```verilog
// Módulo de la unidad aritmético-lógica (ALU)

// Declaración de los puertos de entrada y salida
input [31:0] A, B; // Operandos de 32 bits
input [3:0] Op; // Código de operación de 4 bits
output [31:0] Resultado; // Resultado de la operación

// Tabla de verdad para las operaciones aritméticas y lógicas
always @ (A, B, Op) begin
    case (Op)
        4'b0000: Resultado = A + B; // Suma
        4'b0001: Resultado = A - B; // Resta
        4'b0010: Resultado = A * B; // Multiplicación
        4'b0011: Resultado = A / B; // División
        4'b0100: Resultado = A & B; // Y lógico
        4'b0101: Resultado = A | B; // O lógico
        4'b0110: Resultado = ~A; // Negación
        default: Resultado = 0; // Valor por defecto
    endcase
end

// Módulo de la unidad de control (UC)

// Declaración de los puertos de entrada y salida
input [31:0] Instrucción; // Instrucción de 32 bits
output [3:0] Op; // Código de operación de 4 bits
output [4:0] Reg_Dirección; // Dirección del registro de destino
output [31:0] Dato_Registro; // Dato a escribir en el registro de destino

// Tabla de verdad para las instrucciones
always @ (Instrucción) begin
    case (Instrucción[31:26])
        6'b000000: begin // ADD: suma
            Op = 4'b0000;
            Reg_Dirección = Instrucción[25:21];
            Dato_Registro = A + B;
        end
        6'b000001: begin // SUB: resta
            Op = 4'b0001;
            Reg_Dirección = Instrucción[25:21];
            Dato_Registro = A - B;
        end
        6'b000010: begin // MUL: multiplicación
            Op = 4'b0010;
            Reg_Dirección = Instrucción[25:21];
            Dato_Registro = A * B;
        end
        6'b000011: begin // DIV: división
            Op = 4'b0011;
            Reg_Dirección = Instrucción[25:21];
            Dato_Registro = A / B;
        end
        6'b000100: begin // AND: Y lógico
            Op = 4'b0100;
            Reg_Dirección = Instrucción[25:21];
            Dato_Registro = A & B;
        end
        6'b000101: begin // OR: O lógico
            Op = 4'b0101;
            Reg_Dirección = Instrucción[25:21];
            Dato_Registro = A | B;
        end
        6'b000110: begin // NOT: negación
            Op = 4'b0110;
            Reg_Dirección = Instrucción[25:21];
            Dato_Registro = ~A;
        end
        default: begin // Instrucción no válida
            Op = 4'b0000;
            Reg_Dirección = 5'b00000;
            Dato_Registro = 0;
        end
    endcase
end

// Módulo del procesador (CPU)

// Declaración de los puertos de entrada y salida
input [31:0] Reloj; // Reloj del sistema
input [31:0] Reinicio; // Señal de reinicio

// Registros de la CPU
reg [31:0] A, B; // Registros de uso general
reg [4:0] Reg_Destino; // Registro de destino para la operación
reg [31:0] Dato_Destino; // Dato a escribir en el registro de destino

// Unidad aritmético-lógica (ALU)
wire [31:0] Resultado; // Resultado de la operación

// Unidad de control (UC)
wire [3:0] Op; // Código de operación de 4 bits
wire [4:0] Reg_Dirección; // Dirección del registro de destino
wire [31:0] Dato_Registro; // Dato a escribir en el registro de destino

// Registro de instrucción (RI)
reg [31:0] Instrucción; // Instrucción actual

// Contador de programa (PC)
reg [31:0] PC; // Dirección de la instrucción actual

// Etapa de búsqueda de instrucción (IF)
always @ (posedge Reloj) begin
    if (Reinicio) begin
        PC <= 0; // Reiniciar el contador de programa
        Instrucción <= 0; // Reiniciar el registro de instrucción
    end
    else begin
        PC <= PC + 4; // Incrementar el contador de programa
        Instrucción <= Memoria_Instrucción[PC]; // Leer la instrucción de la memoria
    end
end

// Etapa de decodificación de instrucción (ID)
always @ (Instrucción) begin
    Reg_Destino <= Instrucción[25:21]; // Extraer el registro de destino
    Dato_Destino <= Dato_Registro; // Almacenar el dato en el registro de destino
end

// Etapa de ejecución de instrucción (EX)
always @ (A, B, Op) begin
    Resultado <= ALU(A, B, Op); // Ejecutar la operación en la ALU
end

// Etapa de escritura de registro (WB)
always @ (posedge Reloj) begin
    if (Reg_Destino != 0) begin // Si el registro de destino no es cero
        Registros[Reg_Destino] <= Dato_Destino; // Escribir el dato en el registro de destino
    end
end

// Conexión de los módulos y señales
assign A = Registros[Instrucción[20:16]]; // Leer el primer operando del registro
assign B = Registros[Instrucción[15:11]]; // Leer el segundo operando del registro
assign Op = UC(Instrucción); // Obtener el código de operación de la UC
assign