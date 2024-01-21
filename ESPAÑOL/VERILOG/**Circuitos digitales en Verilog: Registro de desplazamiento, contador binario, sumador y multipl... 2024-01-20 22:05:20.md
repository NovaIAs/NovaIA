```verilog
// Registro de desplazamiento de 8 bits con retroalimentación
module registro_desplazamiento_8_bits (
  input clk,
  input rst,
  input [7:0] data_in,
  output [7:0] data_out
);

// Registro de desplazamiento
reg [7:0] registro;

// Lógica de actualización del registro
always @(posedge clk) begin
  if (rst) begin
    registro <= 8'b0;
  end else begin
    registro <= {registro[6:0], data_in};
  end
end

// Salida del registro
assign data_out = registro;

endmodule

// Contador binario de 4 bits
module contador_binario_4_bits (
  input clk,
  input rst,
  output [3:0] count
);

// Contador
reg [3:0] contador;

// Lógica de actualización del contador
always @(posedge clk) begin
  if (rst) begin
    contador <= 4'b0;
  end else begin
    contador <= contador + 1'b1;
  end
end

// Salida del contador
assign count = contador;

endmodule

// Sumador de 8 bits
module sumador_8_bits (
  input [7:0] a,
  input [7:0] b,
  output [8:0] suma
);

// Suma
wire [8:0] suma_temp;

// Lógica de cálculo de la suma
assign suma_temp = a + b;

// Suma con acarreo
assign suma = {suma_temp[8], suma_temp[7:0]};

endmodule

// Multiplicador de 8 bits
module multiplicador_8_bits (
  input [7:0] a,
  input [7:0] b,
  output [15:0] producto
);

// Registros de multiplicación
reg [7:0] a_reg;
reg [7:0] b_reg;

// Registro de acarreo
reg acarreo;

// Lógica de cálculo del producto
always @(posedge clk) begin
  if (rst) begin
    a_reg <= 8'b0;
    b_reg <= 8'b0;
    acarreo <= 1'b0;
  end else begin
    a_reg <= a;
    b_reg <= b;

    // Multiplicación
    producto <= {acarreo, a_reg * b_reg};

    // Actualización del acarreo
    acarreo <= producto[15];
  end
end

// Salida del producto
assign producto = producto;

endmodule
```

Este código implementa un registro de desplazamiento de 8 bits, un contador binario de 4 bits, un sumador de 8 bits y un multiplicador de 8 bits, todos ellos en Verilog.

El registro de desplazamiento es un circuito que almacena una serie de bits y los desplaza en cada ciclo de reloj. El contador binario es un circuito que cuenta en binario y se puede utilizar para generar señales de control. El sumador es un circuito que suma dos números binarios y el multiplicador es un circuito que multiplica dos números binarios.

Estos circuitos se pueden utilizar para crear sistemas digitales más complejos, como microprocesadores y controladores.