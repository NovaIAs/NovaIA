```verilog
// Módulo principal del diseño digital

// Definición de los módulos
module mux2x1(input [1:0] A, input B, input S, output Y);
  assign Y = (S) ? B : A;
endmodule

module sumador_completo(input A, input B, input Cin, output S, output Cout);
  assign {Cout, S} = A + B + Cin;
endmodule

module sumador_paralelo(input [3:0] A, input [3:0] B, input Cin, output [3:0] S, output Cout);
  wire [3:0] C;
  sumador_completo U0(A[0], B[0], Cin, S[0], C[0]);
  sumador_completo U1(A[1], B[1], C[0], S[1], C[1]);
  sumador_completo U2(A[2], B[2], C[1], S[2], C[2]);
  sumador_completo U3(A[3], B[3], C[2], S[3], Cout);
endmodule

module multiplicador_entero(input [3:0] A, input [3:0] B, output [7:0] P);
  wire [3:0] R0, R1, R2, R3;
  mux2x1 U0(A, 0, B[0], R0);
  mux2x1 U1(A, R0, B[1], R1);
  mux2x1 U2(A, R1, B[2], R2);
  mux2x1 U3(A, R2, B[3], R3);
  sumador_paralelo U4(R0, 0, 0, P[3:0], C);
  sumador_paralelo U5(R1, 0, C, P[7:4], C);
  sumador_paralelo U6(R2, 0, C, , C);
  sumador_paralelo U7(R3, 0, C, P[15:8], C);
endmodule

// Instanciación de los módulos
multiplier U0(A, B, P);

// Fin del módulo principal
endmodule
```

Explicación del código:

1. Módulo `mux2x1`: Es un multiplexor de 2 entradas que selecciona una de las dos entradas en función de un bit de selección `S`.


2. Módulo `sumador_completo`: Implementa un sumador completo de un bit, que suma dos bits de entrada `A` y `B` junto con un bit de acarreo de entrada `Cin`, y produce una suma `S` y un bit de acarreo de salida `Cout`.


3. Módulo `sumador_paralelo`: Implementa un sumador paralelo de 4 bits, que suma dos números binarios de 4 bits `A` y `B` junto con un bit de acarreo de entrada `Cin`, y produce una suma `S` de 4 bits y un bit de acarreo de salida `Cout`.


4. Módulo `multiplicador_entero`: Implementa un multiplicador entero de 4 bits, que multiplica dos números binarios de 4 bits `A` y `B`, y produce un producto `P` de 8 bits.


5. Módulo `principal`: Instancia el módulo `multiplicador_entero` para realizar la multiplicación de dos números binarios de 4 bits `A` y `B`, y asigna el producto `P` a una salida de 8 bits.