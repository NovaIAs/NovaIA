```verilog
// Módulo principal del diseño
module top_module (
    // Entradas
    input clk,
    input reset,
    input [7:0] data_in,
    
    // Salidas
    output [7:0] data_out
);

    // Registro de cambio de 8 bits
    reg [7:0] shift_register;
    
    // Contador de 4 bits
    reg [3:0] counter;
    
    // Máquina de estados
    reg [2:0] state;
    
    // Bloque siempre activo
    always @(*) begin
        case (state)
            3'b000: begin  // Estado inicial
                data_out = 8'b00000000;
                shift_register = 8'b00000000;
                counter = 4'b0000;
            end
            
            3'b001: begin  // Estado de desplazamiento
                shift_register = {shift_register[6:0], data_in[0]};
                counter = counter + 1;
            end
            
            3'b010: begin  // Estado de carga
                data_out = shift_register;
                counter = 4'b0000;
            end
        endcase
    end
    
    // Bloque activado por el reloj
    always @(posedge clk, posedge reset) begin
        if (reset) begin
            state <= 3'b000;
        end else begin
            case (state)
                3'b000: begin
                    if (counter == 4'b1111) begin
                        state <= 3'b001;
                    end
                end
                
                3'b001: begin
                    if (counter == 4'b1111) begin
                        state <= 3'b010;
                    end
                end
                
                3'b010: begin
                    state <= 3'b000;
                end
            endcase
        end
    end
endmodule
```

Explicación del código:

1. El módulo `top_module` tiene tres entradas: `clk` (reloj), `reset` (reinicio) y `data_in` (datos de entrada de 8 bits). También tiene una salida: `data_out` (datos de salida de 8 bits).

2. El módulo contiene tres registros: `shift_register` (registro de cambio de 8 bits), `counter` (contador de 4 bits) y state (máquina de estados de 3 bits).

3. El bloque siempre activo evalúa el estado actual de la máquina de estados y actualiza los valores de los registros `data_out`, `shift_register` y `counter` en consecuencia.

4. El bloque activado por el reloj se ejecuta en cada flanco de subida del reloj (o en el primer flanco de subida después del reinicio). Actualiza el estado de la máquina de estados de acuerdo con el estado actual y el valor del contador.

5. El estado inicial de la máquina de estados es `3'b000`. En este estado, los datos de salida son cero, el registro de cambio se inicializa a cero y el contador se inicializa a cero.

6. El estado `3'b001` es el estado de desplazamiento. En este estado, los datos de entrada se desplazan en el registro de cambio y el contador se incrementa.

7. El estado `3'b010` es el estado de carga. En este estado, los datos del registro de cambio se cargan en los datos de salida y el contador se reinicia.

8. La máquina de estados cambia de estado según el valor del contador. Cuando el contador alcanza el valor `4'b1111`, la máquina de estados pasa del estado `3'b000` al estado `3'b001`, y del estado `3'b001` al estado `3'b010`.

9. El módulo `top_module` implementa un circuito que desplaza los datos de entrada en un registro de cambio, los cuenta y los carga en los datos de salida. Este circuito es útil en muchas aplicaciones, como la comunicación en serie y el procesamiento de señales digitales.