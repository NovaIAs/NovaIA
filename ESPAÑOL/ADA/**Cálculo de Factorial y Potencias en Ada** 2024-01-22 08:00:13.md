```ada
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;

procedure Factorial is
   function Potencia (Base: Integer; Exponente: Integer) return Integer is
   begin
      if Exponente = 0 then
         return 1;
      else
         return Base * Potencia(Base, Exponente - 1);
      end if;
   end Potencia;

   Num: Integer;
   Factorial: Integer;
   begin
      Put("Introduce un número entero positivo: ");
      Get(Num);
      Factorial := 1;
      for I in 1 .. Num loop
         Factorial := Factorial * I;
      end loop;
      Put_Line("El factorial de " & Integer'Image(Num) & " es " &
                Integer'Image(Factorial));
      Put_Line("El resultado de elevar 2 al cuadrado es: " &
                Integer'Image(Potencia(2, 2)));
   end Factorial;
```

Explicación:

- El paquete `Ada.Text_IO` proporciona funciones para la entrada y salida de texto.
- El paquete `Ada.Integer_Text_IO` proporciona funciones para la entrada y salida de enteros.
- La función `Potencia` calcula la potencia de un número dado a un exponente dado.
- El procedimiento `Factorial` calcula el factorial de un número dado.
- El procedimiento `Factorial` utiliza un bucle `for` para calcular el factorial.
- El procedimiento `Factorial` utiliza la función `Put_Line` para imprimir el resultado.
- El procedimiento `Factorial` utiliza la función `Integer'Image` para convertir un entero en una cadena.
- El procedimiento `Factorial` utiliza la función `Get` para leer un entero de la entrada estándar.