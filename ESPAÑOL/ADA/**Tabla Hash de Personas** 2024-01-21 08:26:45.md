```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Complejo is

   type Nombre is array (1..10) of Character;

   type Persona is record
      nombre : Nombre;
      edad   : Integer;
   end record;

   type Lista_Personas is array (1..100) of Persona;

   function Compare_Nombres (a, b : Nombre) return Boolean is
      i : Integer;
   begin
      for i in a'Range loop
         if a(i) /= b(i) then
            return False;
         end if;
      end loop;
      return True;
   end Compare_Nombres;

   function Hash (nombre : Nombre) return Integer is
      h : Integer := 0;
      i : Integer;
   begin
      for i in nombre'Range loop
         h := h + Integer(nombre(i));
      end loop;
      return h mod 100;
   end Hash;

   function Buscar_Persona (nombre : Nombre; lista : Lista_Personas) return Integer is
      i : Integer;
   begin
      i := Hash(nombre);
      while not Compare_Nombres(lista(i).nombre, nombre) loop
         i := (i + 1) mod 100;
      end loop;
      return i;
   end Buscar_Persona;

   procedure Insertar_Persona (persona : Persona; lista : in out Lista_Personas) is
      i : Integer;
   begin
      i := Hash(persona.nombre);
      while not Compare_Nombres(lista(i).nombre, Persona'Val) loop
         i := (i + 1) mod 100;
      end loop;
      lista(i) := persona;
   end Insertar_Persona;

   procedure Eliminar_Persona (nombre : Nombre; lista : in out Lista_Personas) is
      i : Integer;
   begin
      i := Buscar_Persona(nombre, lista);
      while not Compare_Nombres(lista(i).nombre, Persona'Val) loop
         i := (i + 1) mod 100;
      end loop;
      lista(i) := lista(100);
   end Eliminar_Persona;

   procedure Imprimir_Lista (lista : Lista_Personas) is
      i : Integer;
   begin
      for i in lista'Range loop
         Put(lista(i).nombre);
         Put(" ");
         Put(lista(i).edad);
         New_Line;
      end loop;
   end Imprimir_Lista;

begin
   declare
      lista : Lista_Personas;
      num_personas : Integer;
      nombre : Nombre;
      edad  : Integer;
   begin
      num_personas := 0;
      loop
         Get(nombre);
         if nombre(1) = '\n' then
            exit;
         end if;
         Get(edad);
         Insertar_Persona(Persona'(nombre, edad), lista);
         num_personas := num_personas + 1;
      end loop;
      Imprimir_Lista(lista);
   end;
end Complejo;
```

Este código crea una tabla hash de personas, donde cada persona tiene un nombre y una edad. La tabla hash está implementada como un array de 100 elementos, y cada elemento del array es un registro que contiene el nombre y la edad de una persona.

Para insertar una persona en la tabla hash, primero se calcula el hash del nombre de la persona utilizando la función `Hash`. El hash se utiliza para determinar el índice del array donde se insertará la persona. Si el índice ya está ocupado por otra persona, se busca el siguiente índice libre.

Para buscar una persona en la tabla hash, se calcula el hash del nombre de la persona y se utiliza para determinar el índice del array donde se encuentra la persona. Si la persona no se encuentra en el índice, se busca en el siguiente índice hasta que se encuentra la persona o se llega al final del array.

Para eliminar una persona de la tabla hash, se calcula el hash del nombre de la persona y se utiliza para determinar el índice del array donde se encuentra la persona. Se elimina la persona del índice y se mueve la última persona del array al índice.

El código también incluye una función para imprimir la lista de personas en la tabla hash.

Este código es complejo porque utiliza una tabla hash para almacenar y buscar personas. Una tabla hash es una estructura de datos que se utiliza para almacenar y buscar datos de forma eficiente. Una tabla hash utiliza una función hash para calcular el índice del array donde se almacena un dato. Esto hace que la búsqueda de datos en una tabla hash sea muy rápida.

Este código también es complejo porque utiliza una función para comparar nombres. La función `Compare_Nombres` compara dos nombres y devuelve `True` si los nombres son iguales y `False` si los nombres son diferentes. Esto hace que el código sea más flexible porque permite comparar nombres de diferentes tamaños.