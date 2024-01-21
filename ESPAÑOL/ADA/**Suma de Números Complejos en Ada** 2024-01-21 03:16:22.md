```ada
with Ada.Text_IO;     use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Complejo is

   type Cplx is record
      Real : Float;
      Imag : Float;
   end record;

   procedure LeerCplx(X : out Cplx) is
   begin
      Put("Parte real: "); Get(X.Real);
      Put("Parte imaginaria: "); Get(X.Imag);
   end LeerCplx;

   procedure SumaCplx(A, B : in Cplx; Suma : out Cplx) is
   begin
      Suma.Real := A.Real + B.Real;
      Suma.Imag := A.Imag + B.Imag;
   end SumaCplx;

begin
   declare
      C1, C2, Suma : Cplx;
   begin
      Put("Primer complejo:"); LeerCplx(C1);
      Put("Segundo complejo:"); LeerCplx(C2);
      SumaCplx(C1, C2, Suma);
      Put_Line("Suma de complejos:");
      Put("Parte real: "); Put(Suma.Real, 0, 2, 0);
      Put("Parte imaginaria: "); Put(Suma.Imag, 0, 2, 0);
   end;
end Complejo;
```
Este código es un programa en Ada que permite sumar dos números complejos.

El código está dividido en dos partes: la definición de tipos y procedimientos, y el cuerpo del programa.

La definición de tipos y procedimientos incluye la definición del tipo Cplx, que representa un número complejo, y los procedimientos LeerCplx, SumaCplx y Put_Line.

El cuerpo del programa incluye la declaración de las variables C1, C2 y Suma, y la llamada a los procedimientos LeerCplx, SumaCplx y Put_Line.

El procedimiento LeerCplx solicita al usuario que introduzca la parte real e imaginaria de un número complejo, y lo almacena en la variable X.

El procedimiento SumaCplx suma dos números complejos y almacena el resultado en la variable Suma.

El procedimiento Put_Line imprime una línea de texto en la consola.

El código está escrito en español y utiliza la convención de sangría para mejorar la legibilidad.