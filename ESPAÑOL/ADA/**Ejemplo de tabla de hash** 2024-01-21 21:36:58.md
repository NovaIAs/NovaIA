```ada
with Ada.Containers.Vectors;
with Ada.Containers.Hash_Maps;
with Ada.Strings;
with Ada.Strings.Hash;
with Ada.Streams.Stream_IO;

procedure Hash_Table_Example is
   package Hash_Map_Example_Pkg is new Ada.Containers.Hash_Maps(
      Key_Type => Ada.Strings.Hash(20),
      Element_Type => Ada.Strings.String);

   use Hash_Map_Example_Pkg;

   function String_Hash (S : String) return Ada.Strings.Hash is
   begin
      return Ada.Strings.Hash(S);
   end String_Hash;

   type Key_Type is String_Hash;

   function Equals (a, b : Key_Type) return Boolean is
   begin
      return a = b;
   end Equals;

   function Hash (S : String) return Hash_Map_Example_Pkg.Hash is
   begin
      return S'Hash_Value;
   end Hash;

   type Table is Ada.Containers.Hash_Map(String, Integer);

   function Put(T : in out Table; K : in String; V : in Integer) is
   begin
      Ada.Containers.Hash_Map.Insert(T, K, V);
   end Put;

   function Get(T : Table; K : in String) return Integer is
   begin
      return Ada.Containers.Hash_Map.Find(T, K);
   end Get;

begin
   declare
      Table : Table := (<>);
   begin
      Put(Table, "Ada", 10);
      Put(Table, "Pascal", 20);
      Put(Table, "Modula", 30);

      Ada.Streams.Stream_IO.Put_Line("The value of 'Ada' is " &
         Integer'Image(Get(Table, "Ada")));
      Ada.Streams.Stream_IO.Put_Line("The value of 'Pascal' is " &
         Integer'Image(Get(Table, "Pascal")));
      Ada.Streams.Stream_IO.Put_Line("The value of 'Modula' is " &
         Integer'Image(Get(Table, "Modula")));
   end;
end Hash_Table_Example;
```

Explicación:

* El código crea un paquete denominado `Hash_Map_Example_Pkg` que implementa un mapa hash con claves de tipo `Ada.Strings.Hash` y valores de tipo `Ada.Strings.String`.
* La función `String_Hash` calcula el valor hash de una cadena de caracteres.
* El tipo `Key_Type` se define como `String_Hash`, lo que significa que las claves del mapa hash serán valores hash de cadenas de caracteres.
* La función `Equals` compara dos claves del mapa hash y devuelve `True` si son iguales.
* La función `Hash` calcula el valor hash de una cadena de caracteres.
* El tipo `Table` se define como un mapa hash con claves de tipo `String` y valores de tipo `Integer`.
* La función `Put` inserta un par clave-valor en el mapa hash.
* La función `Get` devuelve el valor asociado a una clave en el mapa hash.
* El procedimiento `Hash_Table_Example` crea un mapa hash y lo llena con algunos valores.
* El procedimiento `Hash_Table_Example` utiliza las funciones `Put` y `Get` para insertar y recuperar valores del mapa hash.