Claro! Aqui está um código complexo em ADA:

```
-- Este programa calcula o n-ésimo termo da sequência de Fibonacci

with Ada.Text_IO; use Ada.Text_IO;

procedure Fibonacci is

   function Calcular_Fibonacci(N : Natural) return Natural is
      Fib : array (0..N) of Natural := (0, 1);
   begin
      for I in 2..N loop
         Fib(I) := Fib(I - 1) + Fib(I - 2);
      end loop;

      return Fib(N);
   end Calcular_Fibonacci;

   Numero : Natural;
begin
   Put("Digite o número para calcular o termo correspondente da sequência de Fibonacci: ");
   Get(Numero);

   Put("O termo correspondente é: ");
   Put(Calcular_Fibonacci(Numero));
   New_Line;
end Fibonacci;
```

Este código em ADA permite que você calcule o n-ésimo termo da sequência de Fibonacci. Ele usa uma função recursiva para calcular cada termo da sequência. Basta digitar o número desejado e o programa retornará o termo correspondente.

Espero que este código atenda às suas expectativas!