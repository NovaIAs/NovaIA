```ada
-- Este es un programa complejo en Ada que crea un menú interactivo para que el usuario pueda interactuar con el programa.

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;

procedure Menú_Interactivo is
   type Opción is (Salir, Sumar, Restar, Multiplicar, Dividir);
   función Mostrar_Menú return Opción is
   begin
      Ada.Text_IO.Put_Line("Menú principal:");
      Ada.Text_IO.Put_Line("1. Salir");
      Ada.Text_IO.Put_Line("2. Sumar");
      Ada.Text_IO.Put_Line("3. Restar");
      Ada.Text_IO.Put_Line("4. Multiplicar");
      Ada.Text_IO.Put_Line("5. Dividir");
      return Opción'First;
   end Mostrar_Menú;

   procedure Ejecutar_Opción(opción : Opción) is
   begin
      case opción is
         when Salir => Ada.Text_IO.Put_Line("Saliendo...");
         when Sumar =>
            Ada.Text_IO.Put("Ingrese el primer número: ");
            número1 := Ada.Integer_Text_IO.Get;
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put("Ingrese el segundo número: ");
            número2 := Ada.Integer_Text_IO.Get;
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put("La suma de los números es: ");
            Ada.Integer_Text_IO.Put(número1 + número2);
            Ada.Text_IO.New_Line;
         when Restar =>
            Ada.Text_IO.Put("Ingrese el primer número: ");
            número1 := Ada.Integer_Text_IO.Get;
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put("Ingrese el segundo número: ");
            número2 := Ada.Integer_Text_IO.Get;
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put("La resta de los números es: ");
            Ada.Integer_Text_IO.Put(número1 - número2);
            Ada.Text_IO.New_Line;
         when Multiplicar =>
            Ada.Text_IO.Put("Ingrese el primer número: ");
            número1 := Ada.Integer_Text_IO.Get;
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put("Ingrese el segundo número: ");
            número2 := Ada.Integer_Text_IO.Get;
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put("El producto de los números es: ");
            Ada.Integer_Text_IO.Put(número1 * número2);
            Ada.Text_IO.New_Line;
         when Dividir =>
            Ada.Text_IO.Put("Ingrese el primer número: ");
            número1 := Ada.Integer_Text_IO.Get;
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put("Ingrese el segundo número: ");
            número2 := Ada.Integer_Text_IO.Get;
            Ada.Text_IO.New_Line;
            if número2 = 0 then
               Ada.Text_IO.Put_Line("No se puede dividir por cero");
            else
               Ada.Text_IO.Put("El cociente de los números es: ");
               Ada.Integer_Text_IO.Put(número1 / número2);
               Ada.Text_IO.New_Line;
            end if;
         when others =>
            Ada.Text_IO.Put_Line("Opción no válida");
      end case;
   end Ejecutar_Opción;

begin
   loop
      opción := Mostrar_Menú;
      Ejecutar_Opción(opción);
      if opción = Salir then
         exit;
      end if;
   end loop;
end Menú_Interactivo;
```

Explicación del código:

* El código define un tipo `Opción` que contiene los posibles valores del menú: `Salir`, `Sumar`, `Restar`, `Multiplicar` y `Dividir`.
* La función `Mostrar_Menú` muestra el menú al usuario y devuelve la opción seleccionada.
* El procedimiento `Ejecutar_Opción` ejecuta la opción seleccionada por el usuario.
* El programa principal (`Menú_Interactivo`) crea un bucle infinito que muestra el menú, ejecuta la opción seleccionada y sale del bucle cuando el usuario selecciona la opción `Salir`.

Este código es complejo porque combina varios conceptos de Ada, como tipos, funciones, procedimientos, bucles y condicionales. También utiliza la biblioteca `Ada.Text_IO` para la entrada y salida de datos.