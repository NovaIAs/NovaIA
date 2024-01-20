```ada
with Ada.Text_IO;

procedure Fibonacci es
   -- Declaración de variables
   n_fibonacci: constant Natural := 10;
   fibonacci: array (0 .. n_fibonacci) of Natural;

begin
   fibonacci(0) := 0;
   fibonacci(1) := 1;

   -- Cálculo de la secuencia de Fibonacci
   for i in 2 .. n_fibonacci loop
      fibonacci(i) := fibonacci(i - 1) + fibonacci(i - 2);
   end loop;

   -- Impresión de la secuencia de Fibonacci
   for i in 0 .. n_fibonacci loop
      Ada.Text_IO.Put(fibonacci(i));
      Ada.Text_IO.New_Line;
   end loop;
end Fibonacci;
```

Explicación del código:

* El programa comienza declarando una constante entera n_fibonacci con el valor 10, que representa el número de términos de la secuencia de Fibonacci que se calcularán e imprimirán.


* A continuación, declara una matriz fibonacci que puede contener n_fibonacci + 1 números enteros. Esta matriz se utilizará para almacenar la secuencia de Fibonacci.


* El programa inicializa los dos primeros términos de la secuencia de Fibonacci a 0 y 1, respectivamente.


* Luego, entra en un bucle que calcula los términos restantes de la secuencia de Fibonacci. El bucle itera desde el índice 2 hasta n_fibonacci, y en cada iteración, el término actual de la secuencia de Fibonacci se calcula sumando los dos términos anteriores.


* Una vez que se ha calculado la secuencia de Fibonacci, el programa entra en un bucle que imprime cada término de la secuencia en una línea separada.