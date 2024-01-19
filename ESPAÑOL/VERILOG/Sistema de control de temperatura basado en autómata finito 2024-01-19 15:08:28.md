```verilog
// Módulo superior: Sistema de control de temperatura

module Sistema_Control_Temperatura(
    input  clock,        // Reloj del sistema
    input  reset,        // Señal de reset
    input  temperatura,  // Temperatura actual
    output calentador,   // Señal de control del calentador
    output ventilador     // Señal de control del ventilador
);

// Registrar el estado actual del sistema
reg [1:0] estado_actual;

// Definir los estados del sistema
parameter ESTADO_INICIAL = 2'b00;
parameter ESTADO_CALENTAMIENTO = 2'b01;
parameter ESTADO_ENFRIAMIENTO = 2'b10;

// Definir los parámetros del sistema
parameter TEMPERATURA_OBJETIVO = 25;  // Temperatura objetivo (en grados Celsius)
parameter TEMPERATURA_MINIMA = 18;  // Temperatura mínima permitida (en grados Celsius)
parameter TEMPERATURA_MAXIMA = 30;  // Temperatura máxima permitida (en grados Celsius)

// Bloque siempre para actualizar el estado actual del sistema
always @(posedge clock, posedge reset) begin
    if (reset) begin
        estado_actual <= ESTADO_INICIAL;
    end else begin
        case (estado_actual)
            ESTADO_INICIAL: begin
                if (temperatura < TEMPERATURA_OBJETIVO) begin
                    estado_actual <= ESTADO_CALENTAMIENTO;
                end else if (temperatura > TEMPERATURA_OBJETIVO) begin
                    estado_actual <= ESTADO_ENFRIAMIENTO;
                end
            end
            ESTADO_CALENTAMIENTO: begin
                if (temperatura >= TEMPERATURA_OBJETIVO) begin
                    estado_actual <= ESTADO_INICIAL;
                end
            end
            ESTADO_ENFRIAMIENTO: begin
                if (temperatura <= TEMPERATURA_OBJETIVO) begin
                    estado_actual <= ESTADO_INICIAL;
                end
            end
        endcase
    end
end

// Bloque siempre para generar las señales de control del calentador y el ventilador
always @(posedge clock, posedge reset) begin
    if (reset) begin
        calentador <= 0;
        ventilador <= 0;
    end else begin
        case (estado_actual)
            ESTADO_CALENTAMIENTO: begin
                calentador <= 1;
                ventilador <= 0;
            end
            ESTADO_ENFRIAMIENTO: begin
                calentador <= 0;
                ventilador <= 1;
            end
            default: begin
                calentador <= 0;
                ventilador <= 0;
            end
        endcase
    end
end

endmodule
```

Explicación:

* El módulo `Sistema_Control_Temperatura` es el módulo superior del sistema de control de temperatura.
* El módulo tiene tres entradas: `clock`, `reset` y `temperatura`.
* El módulo tiene dos salidas: `calentador` y `ventilador`.
* El módulo utiliza un registro `estado_actual` para almacenar el estado actual del sistema.
* El módulo define tres estados del sistema: `ESTADO_INICIAL`, `ESTADO_CALENTAMIENTO` y `ESTADO_ENFRIAMIENTO`.
* El módulo define tres parámetros: `TEMPERATURA_OBJETIVO`, `TEMPERATURA_MINIMA` y `TEMPERATURA_MAXIMA`.
* El bloque siempre `always` se utiliza para actualizar el estado actual del sistema en cada ciclo de reloj.
* El bloque siempre `always` se utiliza para generar las señales de control del calentador y el ventilador en cada ciclo de reloj.
* El sistema funciona de la siguiente manera:
    * El sistema comienza en el `ESTADO_INICIAL`.
    * Si la temperatura es menor que la `TEMPERATURA_OBJETIVO`, el sistema pasa al `ESTADO_CALENTAMIENTO`.
    * Si la temperatura es mayor que la `TEMPERATURA_OBJETIVO`, el sistema pasa al `ESTADO_ENFRIAMIENTO`.
    * Cuando la temperatura alcanza la `TEMPERATURA_OBJETIVO`, el sistema vuelve al `ESTADO_INICIAL`.
    * En el `ESTADO_CALENTAMIENTO`, el calentador se enciende y el ventilador se apaga.
    * En el `ESTADO_ENFRIAMIENTO`, el calentador se apaga y el ventilador se enciende.
    * En el `ESTADO_INICIAL`, tanto el calentador como el ventilador están apagados.