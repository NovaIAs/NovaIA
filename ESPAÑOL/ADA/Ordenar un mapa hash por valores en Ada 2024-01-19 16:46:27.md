```ada
with Ada.Containers.Ordered_Maps;         -- Para el tipo Hash_Map
with Ada.Containers.Associative_Arrays;    -- Para el tipo Hash_Map
with Ada.Strings.Hash;                   -- Para el tipo de hash de cadenas
with Ada.Strings.Unbounded;               -- Para el tipo String
with Ada.Characters.Latin_1;             -- Para el tipo Character
with Ada.Text_IO;                        -- Para las operaciones de entrada/salida
with Ada.Integer_Text_IO;                -- Para las operaciones de entrada/salida de números enteros

procedure Ordenar_Mapa(Mapa : in out Hash_Map) is
   -- Función auxiliar para ordenar el mapa por valores
   function Comparar(Clave1, Clave2 : Hash_Map'Key; Valor1, Valor2 : Hash_Map'Element) return Boolean is
   begin
      return Valor1 < Valor2;
   end Comparar;

   Tipo_Ordenado : Ordered_Map(Hash_Map'Key, Hash_Map'Element; Hash_Function => Hash_Map'Hash_Function;
                                                                     Compare     => Comparar);

   Mapa_Ordenado : Tipo_Ordenado;

   -- Copiar el mapa original en el mapa ordenado
   for Clave in Mapa'Domain loop
      Mapa_Ordenado(Clave) := Mapa(Clave);
   end loop;

   -- Limpiar el mapa original
   Mapa.Clear;

   -- Copiar el mapa ordenado en el mapa original
   for Clave in Mapa_Ordenado'Domain loop
      Mapa(Clave) := Mapa_Ordenado(Clave);
   end loop;
end Ordenar_Mapa;

procedure Imprimir_Mapa(Mapa : Hash_Map) is
   -- Función auxiliar para imprimir el mapa
   function Imprimir_Clave(Clave : Hash_Map'Key) is
   begin
      Text_IO.Put(Clave);
   end Imprimir_Clave;

   function Imprimir_Valor(Valor : Hash_Map'Element) is
   begin
      Text_IO.Put(Valor);
   end Imprimir_Valor;

   -- Imprimir el mapa
   Text_IO.Put_Line("Mapa:");
   for Clave in Mapa'Domain loop
      Text_IO.Put("Clave: ");
      Imprimir_Clave(Clave);
      Text_IO.Put(", Valor: ");
      Imprimir_Valor(Mapa(Clave));
      Text_IO.New_Line;
   end loop;
end Imprimir_Mapa;

procedure Main is
   -- Crear un mapa hash
   Mapa : Hash_Map(String, Integer);

   -- Añadir elementos al mapa
   Mapa("Uno") := 1;
   Mapa("Dos") := 2;
   Mapa("Tres") := 3;
   Mapa("Cuatro") := 4;
   Mapa("Cinco") := 5;

   -- Imprimir el mapa original
   Imprimir_Mapa(Mapa);

   -- Ordenar el mapa por valores