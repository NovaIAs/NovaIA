```verilog
// Módulo de prueba para el contador de 4 bits

module contador_prueba;

// Señales de entrada y salida
input clk, rst;
output [3:0] count;

// Instanciar el contador de 4 bits
contador_4bits contador(.clk(clk), .rst(rst), .count(count));

// Generar la señal de reloj
always #10 clk = ~clk;

// Generar la señal de reset
initial begin
    rst = 1'b1;
    #20 rst = 1'b0;
end

// Mostrar el valor del contador en la consola
initial begin
    $monitor("Tiempo = %0t, Contador = %b", $time, count);
    #1000 $finish;
end

endmodule

// Módulo del contador de 4 bits

module contador_4bits(input clk, rst, output [3:0] count);

// Registro del contador
reg [3:0] count;

// Proceso para incrementar el contador
always @(posedge clk) begin
    if (rst) begin
        count <= 4'b0000;
    end else begin
        count <= count + 1'b1;
    end
end

endmodule
```

Explicación del código:

* El módulo `contador_prueba` es un módulo de prueba para el contador de 4 bits. Este módulo genera las señales de reloj y reset, y muestra el valor del contador en la consola.
* El módulo `contador_4bits` es el contador de 4 bits. Este módulo tiene una entrada de reloj, una entrada de reset y una salida de 4 bits que representa el valor del contador.
* El proceso `always @(posedge clk)` es un proceso sensible a la señal de reloj. Este proceso incrementa el valor del contador en 1 cada vez que la señal de reloj cambia de 0 a 1.
* El proceso `initial` es un proceso que se ejecuta una vez al inicio de la simulación. Este proceso genera las señales de reloj y reset, y muestra el valor del contador en la consola.