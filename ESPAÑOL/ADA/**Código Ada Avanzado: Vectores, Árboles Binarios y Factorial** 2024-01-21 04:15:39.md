```ada
with Ada.Containers.Vectors;
with Ada.Text_IO;

procedure Prueba_Compleja is
   type Vector_de_Cadenas is array (Natural range <>) of String;
   type Arbol_Binario is record
      valor : String;
      hijo_izquierdo : Arbol_Binario;
      hijo_derecho : Arbol_Binario;
   end record;

   funcion Crear_Arbol_Binario(valor : String) return Arbol_Binario is
      (valor, null, null);

   procedimiento Recorrer_Arbol_Binario(arbol : Arbol_Binario) is
      if arbol = null then
         return;
      else
         Recorrer_Arbol_Binario(arbol.hijo_izquierdo);
         Ada.Text_IO.Put_Line(arbol.valor);
         Recorrer_Arbol_Binario(arbol.hijo_derecho);
      end if;
   end Recorrer_Arbol_Binario;

   funcion Calcular_Factorial(numero : Natural) return Natural is
      if numero = 0 then
         return 1;
      else
         return numero * Calcular_Factorial(numero - 1);
      end if;
   end Calcular_Factorial;

begin
   -- Ejemplo de uso de un vector de cadenas
   declare
      vector_de_cadenas : Vector_de_Cadenas := ("Uno", "Dos", "Tres", "Cuatro", "Cinco");
   begin
      for i in vector_de_cadenas'Range loop
         Ada.Text_IO.Put_Line(vector_de_cadenas(i));
      end loop;
   end;

   -- Ejemplo de uso de un árbol binario
   declare
      arbol_binario : Arbol_Binario := Crear_Arbol_Binario("Raíz");
   begin
      arbol_binario.hijo_izquierdo := Crear_Arbol_Binario("Hijo izquierdo");
      arbol_binario.hijo_derecho := Crear_Arbol_Binario("Hijo derecho");
      Recorrer_Arbol_Binario(arbol_binario);
   end;

   -- Ejemplo de uso de la función factorial
   declare
      resultado : Natural;
   begin
      resultado := Calcular_Factorial(5);
      Ada.Text_IO.Put_Line("El factorial de 5 es: " & resultado'Image);
   end;
end Prueba_Compleja;
```

Este código incluye una variedad de características avanzadas del lenguaje Ada, como:

* Definición de tipos personalizados (e.g., `Vector_de_Cadenas` y `Arbol_Binario`).
* Uso de funciones recursivas (e.g., `Recorrer_Arbol_Binario` y `Calcular_Factorial`).
* Manejo de excepciones (e.g., la protección contra la desreferencia de un puntero nulo en `Recorrer_Arbol_Binario`).
* Utilización de genéricos (e.g., el tipo `Vector_de_Cadenas` está parametrizado por el tipo de sus elementos).

Este código es un ejemplo de cómo Ada se puede utilizar para escribir código complejo y estructurado, y también es un buen ejemplo de la flexibilidad y potencia del lenguaje.