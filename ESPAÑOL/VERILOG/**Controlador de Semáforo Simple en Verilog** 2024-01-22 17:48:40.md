```verilog
// Diseño de un Controlador de Semáforo Simple en Verilog

// Definiciones de Parámetros
parameter ESTADO_VERDE = 1;
parameter ESTADO_AMARILLO = 2;
parameter ESTADO_ROJO = 3;

// Definiciones de Señales
input  [2:0]  estado_actual;
output [2:0]  estado_siguiente;
output reg    senial_verde;
output reg    senial_amarilla;
output reg    senial_roja;

// Lógica Combinacional
assign estado_siguiente = (estado_actual == ESTADO_VERDE) ? ESTADO_AMARILLO :
                          (estado_actual == ESTADO_AMARILLO) ? ESTADO_ROJO :
                          (estado_actual == ESTADO_ROJO) ? ESTADO_VERDE :
                          ESTADO_VERDE;

// Lógica Secuencial
always @(posedge clk or posedge reset) begin
    if (reset) begin
        senial_verde <= 1'b0;
        senial_amarilla <= 1'b0;
        senial_roja <= 1'b1;
    end else begin
        case (estado_actual)
            ESTADO_VERDE: begin
                senial_verde <= 1'b1;
                senial_amarilla <= 1'b0;
                senial_roja <= 1'b0;
            end

            ESTADO_AMARILLO: begin
                senial_verde <= 1'b0;
                senial_amarilla <= 1'b1;
                senial_roja <= 1'b0;
            end

            ESTADO_ROJO: begin
                senial_verde <= 1'b0;
                senial_amarilla <= 1'b0;
                senial_roja <= 1'b1;
            end
        endcase
    end
end

```

**Explicación:**

Este código Verilog diseña un controlador de semáforo simple.

El código comienza con la definición de parámetros, que en este caso son los estados del semáforo (VERDE, AMARILLO y ROJO).

Luego, el código define las señales que se utilizarán en el diseño. En este caso, tenemos la señal de estado actual, la señal de estado siguiente, y las señales de salida para cada uno de los tres estados del semáforo (VERDE, AMARILLO y ROJO).

La lógica combinacional se utiliza para determinar el estado siguiente del semáforo en función del estado actual. Esto se hace mediante la sentencia `assign`, que asigna el valor de la expresión `(estado_actual == ESTADO_VERDE) ? ESTADO_AMARILLO : (estado_actual == ESTADO_AMARILLO) ? ESTADO_ROJO : (estado_actual == ESTADO_ROJO) ? ESTADO_VERDE : ESTADO_VERDE` a la señal `estado_siguiente`.

La lógica secuencial se utiliza para actualizar las señales de salida del semáforo en función del estado actual y del estado siguiente. Esto se hace mediante el bloque `always`, que se ejecuta cada vez que la señal `clk` o la señal `reset` cambian.

Dentro del bloque `always`, la sentencia `if` se utiliza para comprobar si la señal `reset` está activa. Si es así, las señales de salida del semáforo se inicializan a los valores predeterminados (VERDE apagado, AMARILLO apagado, ROJO encendido).

Si la señal `reset` no está activa, la sentencia `case` se utiliza para comprobar el valor de la señal `estado_actual`. En función del valor de la señal `estado_actual`, las señales de salida del semáforo se actualizan en consecuencia.

Por ejemplo, si la señal `estado_actual` tiene el valor `ESTADO_VERDE`, la señal `senial_verde` se enciende, la señal `senial_amarilla` se apaga y la señal `senial_roja` se apaga.

Este código Verilog se puede utilizar para diseñar un controlador de semáforo simple que cambie de estado (VERDE, AMARILLO y ROJO) en función de un reloj y una señal de reinicio.