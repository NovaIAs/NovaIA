```verilog
// Este código implementa una Unidad Aritmético-Lógica (ALU) de 8 bits en Verilog.

module ALU(
  input [7:0] A, // Primer operando de 8 bits
  input [7:0] B, // Segundo operando de 8 bits
  input [2:0] opcode, // Código de operación de 3 bits
  output [7:0] result, // Resultado de la operación
  output zero // Bandera de cero (1 si el resultado es 0, 0 en caso contrario)
);

// Definir los códigos de operación
parameter ADD = 3'b000; // Suma
parameter SUB = 3'b001; // Resta
parameter AND = 3'b010; // Y lógico
parameter OR = 3'b011; // O lógico
parameter XOR = 3'b100; // XOR
parameter NOT = 3'b101; // Negación
parameter SHIFT_LEFT = 3'b110; // Desplazamiento a la izquierda
parameter SHIFT_RIGHT = 3'b111; // Desplazamiento a la derecha

// Realizar la operación según el código de operación
assign result = (opcode == ADD) ? A + B :
               (opcode == SUB) ? A - B :
               (opcode == AND) ? A & B :
               (opcode == OR) ? A | B :
               (opcode == XOR) ? A ^ B :
               (opcode == NOT) ? ~A :
               (opcode == SHIFT_LEFT) ? A << 1 :
               (opcode == SHIFT_RIGHT) ? A >> 1 :
               0; // Operación por defecto (cero)

// Comprobar si el resultado es cero
assign zero = (result == 0) ? 1 : 0;

endmodule
```

Explicación del código:

* El módulo `ALU` tiene tres entradas: `A` y `B`, que son los operandos de 8 bits, y `opcode`, que es el código de operación de 3 bits.
* El módulo tiene dos salidas: `result`, que es el resultado de la operación, y `zero`, que es una bandera que indica si el resultado es cero.
* En el cuerpo del módulo, se define una lista de parámetros que representan los códigos de operación.
* Se utiliza una instrucción `assign` para asignar el resultado de la operación a la salida `result`. La instrucción `assign` es similar a una asignación en C, pero se utiliza para asignar valores a señales en Verilog.
* La operación que se realiza depende del código de operación. Se utiliza una instrucción `case` para seleccionar la operación adecuada en función del código de operación.
* Se utiliza una instrucción `assign` para asignar el valor de la bandera `zero` a la salida `zero`. La bandera `zero` se pone a 1 si el resultado de la operación es cero, y a 0 en caso contrario.