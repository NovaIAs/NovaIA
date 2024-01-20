```ada
with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;

procedure Fibonacci es
   type Fib_Numbers is array(0..100) of Positive;
   function Calc_Fibonacci(N: Positive) return Positive is
   begin
      declare
         Fib: Fib_Numbers := (others => 0);
      begin
         Fib(0) := 0;
         Fib(1) := 1;
         for I in 2..N loop
            Fib(I) := Fib(I - 1) + Fib(I - 2);
         end loop;
         return Fib(N);
      end;
   end Calc_Fibonacci;

   N: Positive := 0;
   FibN: Positive := 0;
begin
   Put_Line("Introduce un número entero positivo:");
   Get(N);
   if N > 100 then
      Put_Line("El número debe ser menor o igual a 100.");
   elsif N = 0 then
      Put_Line("El número no puede ser cero.");
   else
      FibN := Calc_Fibonacci(N);
      Put("El término ");
      Put(Integer'Image(N));
      Put(" de la serie de Fibonacci es ");
      Put(Integer'Image(FibN));
      New_Line;
   end if;
end Fibonacci;
```

Este código en ADA calcula el n-ésimo número de Fibonacci para un valor entero positivo `N` introducido por el usuario. A continuación te explico el código:

1. Creamos un nuevo tipo de dato llamado `Fib_Numbers`, que es un array de enteros positivos de tamaño 0..100. Esto nos permite almacenar los primeros 100 números de Fibonacci.

2. Definimos una función llamada `Calc_Fibonacci`, que recibe un entero positivo `N` y devuelve el n-ésimo número de Fibonacci. Esta función utiliza el algoritmo recursivo de Fibonacci para calcular el n-ésimo número.

3. Declaramos una variable llamada `N` de tipo `Positive` y la inicializamos a 0. Esta variable se utilizará para almacenar el valor de `N` introducido por el usuario.

4. Declaramos una variable llamada `FibN` de tipo `Positive` y la inicializamos a 0. Esta variable se utilizará para almacenar el n-ésimo número de Fibonacci calculado por la función `Calc_Fibonacci`.

5. El usuario introduce un valor para `N` y se guarda en la variable `N`.

6. Comprobamos si `N` es mayor que 100, si es igual a 0 o si es un número entero positivo. Si `N` es mayor que 100 o igual a 0, se imprime un mensaje de error y el programa termina.

7. Si `N` es un número entero positivo menor que 100, se llama a la función `Calc_Fibonacci` para calcular el n-ésimo número de Fibonacci. El resultado se guarda en la variable `FibN`.

8. Se imprime el n-ésimo número de Fibonacci calculado.