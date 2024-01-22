```verilog
// Módulo contador de 4 bits con entrada de reinicio asíncrono

module contador_4_bits(
    input  clock,         // Reloj de entrada
    input  reset,         // Entrada de reinicio asíncrono
    output [3:0] Q         // Salida del contador
);

    // Registro de 4 bits para almacenar el estado del contador
    reg [3:0] Q;

    // Bloque siempre activo para el reinicio asíncrono
    always @(posedge reset) begin
        // Si la entrada de reinicio es alta, se reinicia el contador a 0
        if (reset) begin
            Q <= 4'b0000;
        end
    end

    // Bloque siempre activo para el contador
    always @(posedge clock) begin
        // Si la entrada de reinicio es baja, se incrementa el contador
        if (!reset) begin
            Q <= Q + 4'b0001;

            // Si el contador llega a 15, se reinicia a 0
            if (Q == 4'b1111) begin
                Q <= 4'b0000;
            end
        end
    end

endmodule
```

Explicación del código:

* El módulo `contador_4_bits` es un contador de 4 bits con entrada de reinicio asíncrono.
* La entrada `clock` es el reloj de entrada del contador.
* La entrada `reset` es la entrada de reinicio asíncrono del contador.
* La salida `Q` es la salida del contador, que es un valor de 4 bits que representa el estado actual del contador.
* El registro `Q` es un registro de 4 bits que almacena el estado actual del contador.
* El bloque siempre activo para el reinicio asíncrono se ejecuta cada vez que la entrada `reset` cambia a alto. Si la entrada `reset` es alta, se reinicia el contador a 0.
* El bloque siempre activo para el contador se ejecuta cada vez que el reloj `clock` cambia a alto. Si la entrada `reset` es baja, se incrementa el contador. Si el contador llega a 15, se reinicia a 0.