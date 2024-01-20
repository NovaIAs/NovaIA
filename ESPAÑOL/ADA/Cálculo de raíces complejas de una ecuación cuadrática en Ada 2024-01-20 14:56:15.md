```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;

procedure Calcula_Raices_Complejas is

   type Complejo is record
      real: Float;
      imag: Float;
   end record;

   subtype Complejo_No_Nulo is Complejo range (-1.0e38, 1.0e38);

   function Suma(X, Y: Complejo_No_Nulo) return Complejo_No_Nulo is
   begin
      return (X.real + Y.real, X.imag + Y.imag);
   end Suma;

   function Resta(X, Y: Complejo_No_Nulo) return Complejo_No_Nulo is
   begin
      return (X.real - Y.real, X.imag - Y.imag);
   end Resta;

   function Producto(X, Y: Complejo_No_Nulo) return Complejo_No_Nulo is
   begin
      return ((X.real * Y.real) - (X.imag * Y.imag),
              (X.real * Y.imag) + (X.imag * Y.real));
   end Producto;

   function Division(X, Y: Complejo_No_Nulo) return Complejo_No_Nulo is
   begin
      if Y.real = 0.0 and Y.imag = 0.0 then
         raise Constraint_Error;
      end if;
      return ((X.real * Y.real) + (X.imag * Y.imag)) /
             ((Y.real ** 2) + (Y.imag ** 2)),
             ((X.imag * Y.real) - (X.real * Y.imag)) /
             ((Y.real ** 2) + (Y.imag ** 2));
   end Division;

   function Conjugado(X: Complejo_No_Nulo) return Complejo_No_Nulo is
   begin
      return (X.real, -X.imag);
   end Conjugado;

   function Abs(X: Complejo_No_Nulo) return Float is
   begin
      return sqrt((X.real ** 2) + (X.imag ** 2));
   end Abs;

   function Arg(X: Complejo_No_Nulo) return Float is
   begin
      return arctan(X.imag / X.real);
   end Arg;

   function Raices_Complejas(A, B, C: Float) return array (Integer range <>) of Complejo_No_Nulo is
      declare
         Delta: Complejo_No_Nulo;
         Raiz_Delta: Complejo_No_Nulo;
      begin
         Delta := Producto(B, B) - (4.0 * A * C);
         if Delta.real = 0.0 and Delta.imag = 0.0 then
            return (Suma(-B / (2.0 * A)) * Complejo'(-1.0, 0.0));
         elsif Delta.real > 0.0 or Delta.imag <> 0.0 then
            Raiz_Delta := sqrt(Delta);
            return (-B / (2.0 * A)) +
                   ((Raiz_Delta / (2.0 * A)) * Complejo'(-1.0, 0.0));
         else
            return (Complejo'(-1.0, 0.0) * Division(-B / (2.0 * A),
                                                          Raiz_Delta / (2.0 * A)));
         end if;
      end Raices_Complejas;

   use Ada.Float_Text_IO, Ada.Integer_Text_IO;
   A: Float := 1.0;
   B: Float := -1.0;
   C: Float := 1.0;
   Raices: array (Integer range <>) of Complejo_No_Nulo;
begin
   Raices := Raices_Complejas(A, B, C);
   Ada.Text_IO.Put("Las raíces complejas de la ecuación son:");
   for I in Raices'Range loop
      Ada.Text_IO.Put("   (");
      Ada.Float_Text_IO.Put(Raices(I).real, 5, 2);
      Ada.Text_IO.Put(", ");
      Ada.Float_Text_IO.Put(Raices(I).imag, 5, 2);
      Ada.Text_IO.Put(")");
   end loop;
   Ada.Text_IO.New_Line;
end Calcula_Raices_Complejas;
```

Este código calcula las raíces complejas de una ecuación cuadrática de la forma `a*x^2 + b*x + c = 0`.

El código utiliza un tipo de datos llamado `Complejo` para representar los números complejos. El tipo `Complejo` tiene dos campos: `real` e `imag`, que representan la parte real y la parte imaginaria del número complejo, respectivamente.

El código también define varias funciones para operar con números complejos, como `Suma`, `Resta`, `Producto`, `Division`, `Conjugado`, `Abs` y `Arg`. Estas funciones se utilizan para calcular las raíces complejas de la ecuación cuadrática.

La función `Raices_Complejas` calcula las raíces complejas de una ecuación cuadrática a partir de los coeficientes `a`, `b` y `c`. La función primero calcula el discriminante de la ecuación cuadrática, que es el valor de `b^2 - 4*a*c`. Si el discriminante es positivo, la ecuación tiene dos raíces reales distintas. Si el discriminante es cero, la ecuación tiene una raíz real doble. Si el discriminante es negativo, la ecuación tiene dos raíces complejas conjugadas.

La función `Raices_Complejas` utiliza las funciones `Producto` y `Division` para calcular las raíces complejas de la ecuación cuadrática. Si el discriminante es negativo, la función `Raices_Complejas` también utiliza la función `Conjugado` para calcular la raíz compleja conjugada de la primera raíz.

El código principal crea una ecuación cuadrática con coeficientes `a = 1.0`, `b = -1.0` y `c = 1.0`. A continuación, el código llama a la función `Raices_Complejas` para calcular las raíces complejas de la ecuación.

Finalmente, el código imprime las raíces complejas de la ecuación en la consola.