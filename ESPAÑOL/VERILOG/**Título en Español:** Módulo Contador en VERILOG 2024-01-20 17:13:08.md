```verilog
// Módulo contador en VERILOG

// Definición de parámetros y variables
parameter N = 8; // Número de bits del contador
reg [N-1:0] contador; // Registro para almacenar el valor del contador
reg clk; // Señal de reloj
reg rst; // Señal de reset

// Bloque siempre para manejar el reloj
always @(posedge clk) begin
    if (rst) begin
        contador <= 0; // Reiniciar el contador a 0
    end else begin
        contador <= contador + 1; // Incrementar el contador en 1
    end
end

// Bloque siempre para manejar el reset
always @(negedge rst) begin
    contador <= 0; // Reiniciar el contador a 0
end

// Módulo principal
module contador_principal(clk, rst, contador);
    input clk, rst;
    output [N-1:0] contador;

    // Instanciar el módulo contador
    contador contador_inst(clk, rst, contador);
endmodule
```

Explicación:

* El código define un módulo contador con N bits, donde N es un parámetro configurable.

* El registro **contador** almacena el valor actual del contador.

* La señal **clk** es la señal de reloj que impulsa el contador.

* La señal **rst** es la señal de reset que reinicia el contador a 0.

* El bloque **always @(posedge clk)** es un bloque siempre que se ejecuta en cada flanco de subida de la señal de reloj. Si la señal de reset es activa, el contador se reinicia a 0. De lo contrario, el contador se incrementa en 1.

* El bloque **always @(negedge rst)** es un bloque siempre que se ejecuta en cada flanco de bajada de la señal de reset. Cuando la señal de reset es activa, el contador se reinicia a 0.

* El módulo **contador_principal** es el módulo principal que instancia el módulo contador y proporciona las señales de reloj y reset.