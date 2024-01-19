```verilog
// Este código Verilog implementa una máquina de estados finitos (FSM) para controlar un sistema de luces de tráfico.

// Definir los estados de la FSM
module maquina_estados_luces_trafico (
  input clk,  // Reloj del sistema
  input reset,  // Señal de reinicio
  output [2:0] luces_trafico  // Señales de salida para las luces de tráfico
);

// Definir los parámetros de la FSM
parameter ESTADO_INICIAL = 3'b000;  // Estado inicial de la FSM
parameter ESTADO_ROJO = 3'b001;  // Estado en el que la luz roja está encendida
parameter ESTADO_AMARILLO = 3'b010;  // Estado en el que la luz amarilla está encendida
parameter ESTADO_VERDE = 3'b100;  // Estado en el que la luz verde está encendida

// Definir las señales internas de la FSM
reg [2:0] estado_actual;  // Estado actual de la FSM
reg [2:0] proximo_estado;  // Próximo estado de la FSM

// Definir el comportamiento de la FSM
always @(posedge clk, posedge reset) begin
  if (reset) begin
    estado_actual <= ESTADO_INICIAL;
  end else begin
    estado_actual <= proximo_estado;
  end
end

always @(*) begin
  case (estado_actual)
    ESTADO_INICIAL: begin
      proximo_estado = ESTADO_ROJO;
    end
    ESTADO_ROJO: begin
      if (tiempo_rojo_expirado) begin
        proximo_estado = ESTADO_AMARILLO;
      end else begin
        proximo_estado = ESTADO_ROJO;
      end
    end
    ESTADO_AMARILLO: begin
      if (tiempo_amarillo_expirado) begin
        proximo_estado = ESTADO_VERDE;
      end else begin
        proximo_estado = ESTADO_AMARILLO;
      end
    end
    ESTADO_VERDE: begin
      if (tiempo_verde_expirado) begin
        proximo_estado = ESTADO_ROJO;
      end else begin
        proximo_estado = ESTADO_VERDE;
      end
    end
    default: begin
      proximo_estado = ESTADO_INICIAL;
    end
  endcase
end

// Definir las salidas de la FSM
always @(*) begin
  case (estado_actual)
    ESTADO_ROJO: begin
      luces_trafico = 3'b100;  // Rojo encendido, ámbar y verde apagados
    end
    ESTADO_AMARILLO: begin
      luces_trafico = 3'b010;  // Ámbar encendido, rojo y verde apagados
    end
    ESTADO_VERDE: begin
      luces_trafico = 3'b001;  // Verde encendido, rojo y ámbar apagados
    end
    default: begin
      luces_trafico = 3'b000;  // Todas las luces apagadas
    end
  endcase
end

endmodule
```

Explicación del código:

* El código Verilog anterior implementa una máquina de estados finitos (FSM) para controlar un sistema de luces de tráfico.
* La FSM tiene cuatro estados: ESTADO_INICIAL, ESTADO_ROJO, ESTADO_AMARILLO y ESTADO_VERDE.
* El estado actual de la FSM se almacena en el registro "estado_actual".
* El próximo estado de la FSM se calcula en el bloque "always @(*)".
* Las salidas de la FSM se calculan en el bloque "always @(*)".
* La FSM funciona de la siguiente manera:
    * En el estado ESTADO_INICIAL, la FSM se inicializa y cambia al estado ESTADO_ROJO.
    * En el estado ESTADO_ROJO, la luz roja está encendida. Cuando el tiempo de rojo expira, la FSM cambia al estado ESTADO_AMARILLO.
    * En el estado ESTADO_AMARILLO, la luz amarilla está encendida. Cuando el tiempo de amarillo expira, la FSM cambia al estado ESTADO_VERDE.
    * En el estado ESTADO_VERDE, la luz verde está encendida. Cuando el tiempo de verde expira, la FSM cambia al estado ESTADO_ROJO.
* Las señales de salida de la FSM se utilizan para controlar las luces de tráfico.