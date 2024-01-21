```ada
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Containers.Generic_Arrays;

procedure Cubos_con_Digitos_Pares is

   type Tira_de_Digitos is array(Positive range <>) of Positive;

   function Tamano_Tira_de_Digitos(Tira: Tira_de_Digitos) return Positive is
   begin
      return Tira'Length;
   end Tamano_Tira_de_Digitos;

   function Construye_Tira_Desde_Numero(N: Positive) return Tira_de_Digitos is
   begin
      declare
         Tira: Tira_de_Digitos(1..Tamano_Tira_de_Digitos(N));
      begin
         for I in Tira'Range loop
            Tira(I) := N mod 10;
            N := N / 10;
         end loop;
         return Tira;
      end Construye_Tira_Desde_Numero;
   end Construye_Tira_Desde_Numero;

   function Cubo_de_Tira_de_Digitos(Tira: Tira_de_Digitos) return Positive is
   begin
      declare
         Cubo: Positive := 0;
      begin
         for I in Tira'Range loop
            Cubo := Cubo * 10 + Tira(I) ** 3;
         end loop;
         return Cubo;
      end Cubo_de_Tira_de_Digitos;
   end Cubo_de_Tira_de_Digitos;

   function Tira_de_Digitos_Pares(Tira: Tira_de_Digitos) return Boolean is
   begin
      declare
         Es_Par: Boolean := True;
      begin
         for I in Tira'Range loop
            if Tira(I) mod 2 /= 0 then
               Es_Par := False;
               exit;
            end if;
         end loop;
         return Es_Par;
      end Tira_de_Digitos_Pares;
   end Tira_de_Digitos_Pares;

   function Son_Cubos_Distintos(Cubos: Ada.Containers.Generic_Arrays.Array(Positive)) return Boolean is
   begin
      declare
         Son_Distintos: Boolean := True;
      begin
         for I in Cubos'Range loop
            for J in I+1..Cubos'High loop
               if Cubos(I) = Cubos(J) then
                  Son_Distintos := False;
                  exit;
               end if;
            end loop;
         end loop;
         return Son_Distintos;
      end Son_Cubos_Distintos;
   end Son_Cubos_Distintos;

   function Genera_Tira_de_Digitos_Pares_Aleatoria(Longitud: Positive) return Tira_de_Digitos is
   begin
      declare
         Tira: Tira_de_Digitos(1..Longitud);
         Aleatorio: Positive;
      begin
         for I in Tira'Range loop
            Aleatorio := Ada.Random_Numbers.Random(1000);
            loop
               if Aleatorio mod 2 = 0 then
                  Tira(I) := Aleatorio;
                  exit;
               else
                  Aleatorio := Ada.Random_Numbers.Random(1000);
               end if;
            end loop;
         end loop;
         return Tira;
      end Genera_Tira_de_Digitos_Pares_Aleatoria;
   end Genera_Tira_de_Digitos_Pares_Aleatoria;

   function Busca_Tira_de_Digitos_Pares_Aleatoria_con_Cubo_Distinto(Longitud: Positive) return Tira_de_Digitos is
   begin
      declare
         Tira: Tira_de_Digitos;
         Cubos: Ada.Containers.Generic_Arrays.Array(Positive) := (others => 0);
         Cubo: Positive;
      begin
         loop
            Tira := Genera_Tira_de_Digitos_Pares_Aleatoria(Longitud);
            Cubo := Cubo_de_Tira_de_Digitos(Tira);
            Cubos(Cubos'First + Cubos'Last) := Cubo;
            if Son_Cubos_Distintos(Cubos) then
               return Tira;
            end if;
         end loop;
      end Busca_Tira_de_Digitos_Pares_Aleatoria_con_Cubo_Distinto;
   end Busca_Tira_de_Digitos_Pares_Aleatoria_con_Cubo_Distinto;

begin
   Ada.Text_IO.Put_Line("Tira de Dígitos Pares Aleatoria con Cubo Distinto:");
   Ada.Text_IO.Put_Line(Ada.Strings.Fixed.Image(Busca_Tira_de_Digitos_Pares_Aleatoria_con_Cubo_Distinto(5)));
end Cubos_con_Digitos_Pares;
```

Este código está diseñado para generar una tira de dígitos pares aleatoria de una longitud dada y encontrar el cubo de dicha tira. Luego comprueba si el cubo es distinto de todos los cubos de las tiras de dígitos pares generadas anteriormente. El código se compone de las siguientes funciones:

* `Tamano_Tira_de_Digitos`: Calcula el tamaño de una tira de dígitos.
* `Construye_Tira_Desde_Numero`: Convierte un número en una tira de dígitos.
* `Cubo_de_Tira_de_Digitos`: Calcula el cubo de una tira de dígitos.
* `Tira_de_Digitos_Pares`: Comprueba si una tira de dígitos es par.
* `Son_Cubos_Distintos`: Comprueba si un conjunto de cubos son distintos entre sí.
* `Genera_Tira_de_Digitos_Pares_Aleatoria`: Genera una tira de dígitos pares aleatoria de una longitud dada.
* `Busca_Tira_de_Digitos_Pares_Aleatoria_con_Cubo_Distinto`: Encuentra una tira de dígitos pares aleatoria de una longitud dada y con un cubo distinto de todos los cubos de las tiras de dígitos pares generadas anteriormente.

El código principal llama a la función `Busca_Tira_de_Digitos_Pares_Aleatoria_con_Cubo_Distinto` para generar una tira de dígitos pares aleatoria de longitud 5 y con un cubo distinto de todos los cubos de las tiras de dígitos pares generadas anteriormente. Luego imprime la tira de dígitos generada.