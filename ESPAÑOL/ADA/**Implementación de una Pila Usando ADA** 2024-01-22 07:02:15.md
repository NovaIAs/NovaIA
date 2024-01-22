```ada
-- Definición del tipo Pila
type Pila is array (Positive range <>) of Integer;

-- Función para crear una pila vacía
function Crear_Pila return Pila is
begin
   return Pila'Access := (others => 0);
end Crear_Pila;

-- Procedimiento para insertar un elemento en la pila
procedure Insertar (Pila : in out Pila; Elemento : in Integer) is
begin
   Pila(Pila'Last + 1) := Elemento;
   Pila'Last := Pila'Last + 1;
end Insertar;

-- Función para extraer un elemento de la pila
function Extraer (Pila : in out Pila) return Integer is
begin
   if Pila'Last = 0 then
      raise IndexError;
   else
      Pila'Last := Pila'Last - 1;
      return Pila(Pila'Last + 1);
   end if;
end Extraer;

-- Procedimiento para mostrar la pila
procedure Mostrar_Pila (Pila : in Pila) is
begin
   for I in Pila'Range loop
      Put (Pila(I));
   end loop;
   New_Line;
end Mostrar_Pila;

-- Bloque principal
with Ada.Text_IO;
use Ada.Text_IO;

Pila := Crear_Pila;
for I in 1..10 loop
   Insertar (Pila, I);
end loop;

Mostrar_Pila (Pila);

for I in 1..5 loop
   Extraer (Pila);
end loop;

Mostrar_Pila (Pila);
```

Explicación:

* El tipo Pila es un tipo definido por el usuario que representa una pila. Es un array de enteros con un índice positivo.
* La función Crear_Pila crea una nueva pila vacía.
* El procedimiento Insertar inserta un nuevo elemento en la pila.
* La función Extraer extrae el último elemento de la pila y lo devuelve.
* El procedimiento Mostrar_Pila muestra todos los elementos de la pila.
* El bloque principal crea una nueva pila, inserta 10 elementos en ella, muestra la pila, extrae 5 elementos de la pila y muestra la pila de nuevo.