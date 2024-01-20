```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Complejo is
   type T_Complejo is record
      Real : Float;
      Imag : Float;
   end record;

   function Complex (Real : Float; Imag : Float) return T_Complejo is
   begin
      return (Real => Real, Imag => Imag);
   end Complex;

   function Suma (A, B : T_Complejo) return T_Complejo is
   begin
      return (Real => A.Real + B.Real, Imag => A.Imag + B.Imag);
   end Suma;

   function Resta (A, B : T_Complejo) return T_Complejo is
   begin
      return (Real => A.Real - B.Real, Imag => A.Imag - B.Imag);
   end Resta;

   function Producto (A, B : T_Complejo) return T_Complejo is
   begin
      return (Real => A.Real * B.Real - A.Imag * B.Imag,
              Imag => A.Real * B.Imag + A.Imag * B.Real);
   end Producto;

   function Division (A, B : T_Complejo) return T_Complejo is
   begin
      return (Real => (A.Real * B.Real + A.Imag * B.Imag) / (B.Real**2 + B.Imag**2),
              Imag => (A.Imag * B.Real - A.Real * B.Imag) / (B.Real**2 + B.Imag**2));
   end Division;

   function Conjugado (A : T_Complejo) return T_Complejo is
   begin
      return (Real => A.Real, Imag => -A.Imag);
   end Conjugado;

   function Modulo (A : T_Complejo) return Float is
   begin
      return Sqrt (A.Real**2 + A.Imag**2);
   end Modulo;

   function Argumento (A : T_Complejo) return Float is
   begin
      return Atan2 (A.Imag, A.Real);
   end Argumento;

begin
   declare
      A, B : T_Complejo;
   begin
      Put ("Introduzca el primer número complejo (Real e Imaginario): ");
      Get (A.Real); Get (A.Imag);

      Put ("Introduzca el segundo número complejo (Real e Imaginario): ");
      Get (B.Real); Get (B.Imag);

      Put_Line ("La suma de los dos números complejos es: ");
      Put (Suma (A, B).Real); Put (" + ");
      Put (Suma (A, B).Imag); Put ("i");
      New_Line;

      Put_Line ("La resta de los dos números complejos es: ");
      Put (Resta (A, B).Real); Put (" + ");
      Put (Resta (A, B).Imag); Put ("i");
      New_Line;

      Put_Line ("El producto de los dos números complejos es: ");
      Put (Producto (A, B).Real); Put (" + ");
      Put (Producto (A, B).Imag); Put ("i");
      New_Line;

      Put_Line ("La división de los dos números complejos es: ");
      Put (Division (A, B).Real); Put (" + ");
      Put (Division (A, B).Imag); Put ("i");
      New_Line;

      Put_Line ("El conjugado del primer número complejo es: ");
      Put (Conjugado (A).Real); Put (" + ");
      Put (Conjugado (A).Imag); Put ("i");
      New_Line;

      Put_Line ("El módulo del primer número complejo es: ");
      Put (Modulo (A));
      New_Line;

      Put_Line ("El argumento del primer número complejo es: ");
      Put (Argumento (A));
      New_Line;
   end;
end Complejo;
```

El código anterior es un programa complejo en ADA que realiza operaciones aritméticas con números complejos. El programa está dividido en varios módulos, cada uno de los cuales implementa una funcionalidad específica.

El módulo principal del programa es el procedimiento `Complejo`, que crea una variable de tipo `T_Complejo` y llama a las funciones `Suma`, `Resta`, `Producto`, `Division`, `Conjugado`, `Modulo` y `Argumento` para realizar las operaciones aritméticas con los números complejos.

El módulo `T_Complejo` define el tipo de datos `T_Complejo`, que representa un número complejo. El tipo de datos `T_Complejo` consta de dos campos, `Real` e `Imag`, que representan la parte real y la parte imaginaria del número complejo, respectivamente.

El módulo `Complex` implementa las funciones `Suma`, `Resta`, `Producto`, `Division`, `Conjugado`, `Modulo` y `Argumento`, que realizan las operaciones aritméticas con números complejos. Las funciones `Suma`, `Resta` y `Producto` realizan las operaciones de suma, resta y producto de dos números complejos, respectivamente. La función `Division` realiza la operación de división de dos números complejos. La función `Conjugado` devuelve el conjugado de un número complejo. La función `Modulo` devuelve el módulo de un número complejo. La función `Argumento` devuelve el argumento de un número complejo.

El programa `Complejo` solicita al usuario que introduzca dos números complejos. A continuación, llama a las funciones `Suma`, `Resta`, `Producto`, `Division`, `Conjugado`, `Modulo` y `Argumento` para realizar las operaciones aritméticas con los dos números complejos. Finalmente, el programa muestra los resultados de las operaciones aritméticas en la consola.