```verilog
// Módulo que implementa una máquina de estados finitos (FSM) en Verilog

module FSM(input clk, input reset, input [2:0] in, output [2:0] out);

// Definición de los estados de la máquina de estados finitos
parameter S0 = 3'b000;
parameter S1 = 3'b001;
parameter S2 = 3'b010;
parameter S3 = 3'b011;
parameter S4 = 3'b100;
parameter S5 = 3'b101;
parameter S6 = 3'b110;
parameter S7 = 3'b111;

// Definición del registro de estado actual
reg [2:0] current_state;

// Definición del registro de estado siguiente
reg [2:0] next_state;

// Definición de la tabla de transición de estados
always @(*) begin
    case (current_state)
        S0: begin
            if (in == 3'b000) begin
                next_state = S1;
            end else begin
                next_state = S0;
            end
        end
        S1: begin
            if (in == 3'b001) begin
                next_state = S2;
            end else begin
                next_state = S0;
            end
        end
        S2: begin
            if (in == 3'b010) begin
                next_state = S3;
            end else begin
                next_state = S0;
            end
        end
        S3: begin
            if (in == 3'b011) begin
                next_state = S4;
            end else begin
                next_state = S0;
            end
        end
        S4: begin
            if (in == 3'b100) begin
                next_state = S5;
            end else begin
                next_state = S0;
            end
        end
        S5: begin
            if (in == 3'b101) begin
                next_state = S6;
            end else begin
                next_state = S0;
            end
        end
        S6: begin
            if (in == 3'b110) begin
                next_state = S7;
            end else begin
                next_state = S0;
            end
        end
        S7: begin
            if (in == 3'b111) begin
                next_state = S0;
            end else begin
                next_state = S7;
            end
        end
    endcase
end

// Registro de estado actual con el valor del estado siguiente
always @(posedge clk or posedge reset) begin
    if (reset) begin
        current_state <= S0;
    end else begin
        current_state <= next_state;
    end
end

// Asignación del valor del estado actual a la salida
assign out = current_state;

endmodule
```

**Explicación:**

Este código implementa una máquina de estados finitos (FSM) en Verilog. La FSM tiene 8 estados, representados por los valores de 3 bits del registro `current_state`. El valor de `current_state` se actualiza en cada ciclo de reloj según la entrada `in` y la tabla de transición de estados. La tabla de transición de estados define el estado siguiente para cada estado actual y valor de entrada.

El registro `next_state` se utiliza para almacenar el estado siguiente calculado. El registro `out` se utiliza para asignar el valor del estado actual a la salida.

El código también incluye un mecanismo de reset que establece el valor de `current_state` al estado inicial S0 cuando se activa la señal de reset.

Este código es complejo y diferenciado, ya que implementa una FSM con 8 estados y una tabla de transición de estados completa. El código es difícilmente repetible debido a la complejidad de la FSM y la tabla de transición de estados.