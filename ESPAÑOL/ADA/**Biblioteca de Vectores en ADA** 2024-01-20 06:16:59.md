```ada
-- Paquete que define tipos y funciones para operar con vectores de enteros.

package Vectores is

   type Vector is array (Integer range <>) of Integer;

   -- Función que devuelve un vector con los primeros n números naturales.
   function Nats (n : Integer) return Vector is
   begin
      declare
         v : Vector (1 .. n);
      begin
         for i in v'Range loop
            v (i) := i;
         end loop;
         return v;
      end;
   end Nats;

   -- Función que devuelve un vector con los primeros n números pares.
   function Pares (n : Integer) return Vector is
   begin
      declare
         v : Vector (1 .. n);
      begin
         for i in v'Range loop
            v (i) := 2 * i;
         end loop;
         return v;
      end;
   end Pares;

   -- Función que devuelve un vector con los primeros n números impares.
   function Impares (n : Integer) return Vector is
   begin
      declare
         v : Vector (1 .. n);
      begin
         for i in v'Range loop
            v (i) := 2 * i + 1;
         end loop;
         return v;
      end;
   end Impares;

   -- Función que devuelve un vector con los primeros n números primos.
   function Primos (n : Integer) return Vector is
   begin
      declare
         v : Vector (1 .. n);
         p : Integer := 2;
         i : Integer := 1;
      begin
         while i <= n loop
            if EsPrimo (p) then
               v (i) := p;
               i := i + 1;
            end if;
            p := p + 1;
         end loop;
         return v;
      end;
   end Primos;

   -- Función que devuelve un vector con los primeros n números perfectos.
   function Perfectos (n : Integer) return Vector is
   begin
      declare
         v : Vector (1 .. n);
         p : Integer := 28;
         i : Integer := 1;
      begin
         while i <= n loop
            if EsPerfecto (p) then
               v (i) := p;
               i := i + 1;
            end if;
            p := p + 1;
         end loop;
         return v;
      end;
   end Perfectos;

   -- Función que devuelve un vector con los primeros n números de Fibonacci.
   function Fibos (n : Integer) return Vector is
   begin
      declare
         v : Vector (1 .. n);
      begin
         v (1) := 0;
         v (2) := 1;
         for i in 3 .. n loop
            v (i) := v (i - 1) + v (i - 2);
         end loop;
         return v;
      end;
   end Fibos;

   -- Función que devuelve un vector con los primeros n términos de la serie de Catalan.
   function Catalans (n : Integer) return Vector is
   begin
      declare
         v : Vector (0 .. n);
      begin
         v (0) := 1;
         for i in 1 .. n loop
            v (i) := 0;
            for j in 0 .. i - 1 loop
               v (i) := v (i) + v (j) * v (i - j - 1);
            end loop;
         end loop;
         return v;
      end;
   end Catalans;

private

   -- Función auxiliar que determina si un número es primo.
   function EsPrimo (n : Integer) return Boolean is
   begin
      declare
         i : Integer;
      begin
         if n <= 1 then
            return False;
         end if;
         for i in 2 .. n / 2 loop
            if n mod i = 0 then
               return False;
            end if;
         end loop;
         return True;
      end;
   end EsPrimo;

   -- Función auxiliar que determina si un número es perfecto.
   function EsPerfecto (n : Integer) return Boolean is
   begin
      declare
         i : Integer;
         suma : Integer := 0;
      begin
         for i in 1 ..