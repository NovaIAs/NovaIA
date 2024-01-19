```ada
with Ada.Text_IO;

procedure Main is
   type Opcion is (Suma, Resta, Multiplicacion, Division);
   type Resultado is record
      Valor : Integer;
      Opcion : Opcion;
   end record;

   function Calcular(Opcion : Opcion; A, B : Integer) return Resultado is
   begin
      case Opcion is
         when Suma => return Resultado' (A + B, Suma);
         when Resta => return Resultado' (A - B, Resta);
         when Multiplicacion => return Resultado' (A * B, Multiplicacion);
         when Division =>
            if B = 0 then
               raise Constraint_Error;
            else
               return Resultado' (A / B, Division);
            end if;
      end case;
   end Calcular;

   A, B : Integer;
   Opcion : Opcion;
   Resultado : Resultado;

begin
   Ada.Text_IO.Put("Ingrese el primer número:");
   Ada.Text_IO.Get(A);

   Ada.Text_IO.Put("Ingrese el segundo número:");
   Ada.Text_IO.Get(B);

   Ada.Text_IO.Put("Ingrese la operación (1 para suma, 2 para resta, 3 para multiplicación, 4 para división):");
   Ada.Text_IO.Get(Opcion);

   Resultado := Calcular(Opcion, A, B);

   case Resultado.Opcion is
      when Suma => Ada.Text_IO.Put_Line("La suma es " & Integer'Image(Resultado.Valor));
      when Resta => Ada.Text_IO.Put_Line("La resta es " & Integer'Image(Resultado.Valor));
      when Multiplicacion => Ada.Text_IO.Put_Line("La multiplicación es " & Integer'Image(Resultado.Valor));
      when Division => Ada.Text_IO.Put_Line("La división es " & Float'Image(Resultado.Valor));
   end case;
end Main;
```

Explicación:

El código anterior es un programa en Ada que calcula el resultado de una operación aritmética entre dos números enteros. El programa solicita al usuario que introduzca los dos números y la operación que desea realizar (suma, resta, multiplicación o división). A continuación, el programa utiliza una función llamada `Calcular` para realizar la operación y obtener el resultado. El resultado se muestra entonces en la consola.

El código utiliza varios conceptos de Ada, incluyendo:

* Tipos definidos por el usuario: El tipo `Opcion` define las diferentes operaciones aritméticas que el programa puede realizar. El tipo `Resultado` define el resultado de una operación aritmética.
* Funciones: La función `Calcular` realiza la operación aritmética entre dos números y devuelve el resultado.
* Instrucciones `case`: La instrucción `case` se utiliza para seleccionar la operación aritmética que se debe realizar en función del valor de la variable `Opcion`.
* Excepciones: La instrucción `raise` se utiliza para generar una excepción si el usuario intenta realizar una división por cero.

El código anterior es sólo un ejemplo de cómo se puede utilizar Ada para crear programas complejos. Ada es un lenguaje de programación muy potente y versátil que se puede utilizar para crear una amplia variedad de aplicaciones.