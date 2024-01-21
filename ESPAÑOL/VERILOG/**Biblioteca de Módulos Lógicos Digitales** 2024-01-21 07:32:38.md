```verilog
// Módulo para implementar un contador de 4 bits con reinicio síncrono
module contador_4bits(
    input clk,      // Reloj de entrada
    input reset,    // Señal de reinicio síncrono
    output [3:0] Q // Salida del contador
);

// Registro de 4 bits para almacenar el contador
reg [3:0] Q_reg;

// Actualización del contador
always @(posedge clk) begin
    if (reset) begin
        // Reinicio del contador
        Q_reg <= 4'b0000;
    end else begin
        // Incremento del contador
        Q_reg <= Q_reg + 1;
    end
end

// Asignación de la salida del contador
assign Q = Q_reg;

endmodule

// Módulo para implementar un sumador de 4 bits
module sumador_4bits(
    input [3:0] A, // Primer número a sumar
    input [3:0] B, // Segundo número a sumar
    output [3:0] S, // Suma de los dos números
    output C       // Acarreo de la suma
);

// Suma de los bits individuales
wire [3:0] suma;
assign suma = A + B;

// Generación del acarreo
wire [2:0] acarreo;
assign acarreo[0] = suma[0];
assign acarreo[1] = suma[1] | (acarreo[0] & A[1]);
assign acarreo[2] = suma[2] | (acarreo[1] & A[2]);

// Asignación de la suma y el acarreo
assign S = suma[3:0];
assign C = acarreo[2];

endmodule

// Módulo para implementar un registro de desplazamiento de 8 bits con carga paralela
module registro_desplazamiento_8bits(
    input clk,        // Reloj de entrada
    input [7:0] D,     // Datos de entrada
    input load,       // Señal de carga paralela
    input shift,      // Señal de desplazamiento
    output [7:0] Q    // Salida del registro de desplazamiento
);

// Registro de 8 bits para almacenar los datos
reg [7:0] Q_reg;

// Actualización del registro de desplazamiento
always @(posedge clk) begin
    if (load) begin
        // Carga paralela de los datos
        Q_reg <= D;
    end else if (shift) begin
        // Desplazamiento de los datos
        Q_reg <= {Q_reg[6:0], Q_reg[7]};
    end
end

// Asignación de la salida del registro de desplazamiento
assign Q = Q_reg;

endmodule

// Módulo para implementar un contador binario ascendente/descendente de 4 bits
module contador_binario_4bits(
    input clk,         // Reloj de entrada
    input reset,       // Señal de reinicio síncrono
    input up_down,     // Señal de dirección ascendente/descendente
    output [3:0] Q    // Salida del contador
);

// Registro de 4 bits para almacenar el contador
reg [3:0] Q_reg;

// Actualización del contador
always @(posedge clk) begin
    if (reset) begin
        // Reinicio del contador
        Q_reg <= 4'b0000;
    end else begin
        if (up_down) begin
