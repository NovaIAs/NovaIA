```ada
with Ada.Text_IO;

procedure Calcular_Primos is
   type Primos_1000 is array (1..1000) of Integer;
   Primos : Primos_1000 := (others => 0);    -- Todos los números inicialmente marcados como no primos
   Posicion : Integer := 1;
begin
   for Numero in 2..1000 loop
      if Primos(Numero) = 0 then
         -- El número es primo
         Primos(Numero) := 1;
         Posicion := Posicion + 1;
         -- Marcar múltiplos de Numero como no primos
         for Multiplo in Numero*2..1000 by Numero loop
            Primos(Multiplo) := 2;
         end loop;
      end if;
   end loop;

   -- Imprimir los números primos
   Ada.Text_IO.Put_Line("Números primos hasta 1000:");
   for i in 1..Posicion-1 loop
      Ada.Text_IO.Put(Integer'Image(Primos(i)));
      Ada.Text_IO.New_Line;
   end loop;
end Calcular_Primos;
```

Explicación del código:

* La primera línea del código importa la biblioteca `Ada.Text_IO`, que proporciona funciones para la entrada y salida de texto.
* A continuación, se define un tipo personalizado llamado `Primos_1000`, que es una matriz de 1000 enteros. Los valores de esta matriz se utilizarán para almacenar los números primos hasta 1000.
* La siguiente línea crea una variable llamada `Primos` de tipo `Primos_1000`. Todos los valores de esta variable se inicializan a 0, lo que significa que inicialmente todos los números se consideran no primos.
* La variable `Posicion` se utiliza para mantener un registro de la posición actual en la matriz `Primos`.
* El bucle `for` siguiente itera sobre todos los números del 2 al 1000.
* Dentro del bucle `for`, se comprueba si el número actual es primo. Si lo es, se marca como primo (es decir, se establece `Primos(Numero)` a 1) y se incrementa la variable `Posicion`.
* A continuación, se utiliza otro bucle `for` para marcar todos los múltiplos del número actual como no primos.
* Una vez que el bucle `for` ha terminado, se han marcado todos los números primos hasta 1000.
* Por último, se utiliza un bucle `for` para imprimir todos los números primos en la consola.

Este código es un ejemplo de un programa simple pero eficiente para encontrar los números primos hasta un límite dado. El algoritmo utilizado es el algoritmo de la criba de Eratóstenes, que es uno de los más antiguos y conocidos algoritmos para encontrar números primos.