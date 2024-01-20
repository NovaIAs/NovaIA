```ada
with Ada.Containers.Doubly_Linked_Lists;
use  Ada.Containers.Doubly_Linked_Lists;

procedure Ciencia_Ficcion is
   type Planeta is (Tierra, Marte, Júpiter, Saturno, Urano, Neptuno, Plutón);
   type Nave_Espacial is (Halcón_Milenario, Enterprise, Serenity, Voyager, Curiosity, Opportunity);

   procedure Mostrar_Planetas (Planetas : List_Iterator [Planeta]) is
   begin
      loop
         Ada.Text_IO.Put (Planetas.Current);
         Ada.Text_IO.New_Line;
         if Planetas.Last then
            exit;
         end if;
         Planetas.Next;
      end loop;
   end Mostrar_Planetas;

   procedure Mostrar_Naves_Espaciales (Naves_Espaciales : List_Iterator [Nave_Espacial]) is
   begin
      loop
         Ada.Text_IO.Put (Naves_Espaciales.Current);
         Ada.Text_IO.New_Line;
         if Naves_Espaciales.Last then
            exit;
         end if;
         Naves_Espaciales.Next;
      end loop;
   end Mostrar_Naves_Espaciales;

   type Mision is (Exploración, Rescate, Suministros);
   type Tripulante is (Capitán, Copiloto, Ingeniero, Médica, Científico);

   type Expedición is record
      Nombre         : String(1..20);
      Destino        : Planeta;
      Nave_Espacial  : Nave_Espacial;
      Mision         : Mision;
   end record;

   type Lista_Expediciones is new List [Expedición];
   Lista_Expediciones : Lista_Expediciones := new Lista_Expediciones;

   Nombre_Expedición : String(1..35);
   Nave_Espacial_Expedición : Nave_Espacial;
   Mision_Expedición : Mision;
   Planeta_Destino_Expedición : Planeta;

   procedure Crear_Expedición is
   begin
      Ada.Text_IO.Put ("Nombre de la expedición: ");
      Ada.Text_IO.Get_Line (Nombre_Expedición);

      Ada.Text_IO.Put ("Nave espacial: ");
      Ada.Text_IO.Get (Nave_Espacial_Expedición);

      Ada.Text_IO.Put ("Mision: ");
      Ada.Text_IO.Get (Mision_Expedición);

      Ada.Text_IO.Put ("Planeta destino: ");
      Ada.Text_IO.Get (Planeta_Destino_Expedición);

      Lista_Expediciones.Insert (Expedición' (Nombre_Expedición, Planeta_Destino_Expedición,
                                                    Nave_Espacial_Expedición, Mision_Expedición));
   end Crear_Expedición;

   procedure Mostrar_Expediciones is
   begin
      Ada.Text_IO.Put_Line ("Expediciones:");
      if Lista_Expediciones.IsEmpty then
         Ada.Text_IO.Put_Line ("No hay expediciones programadas.");
      else
         Mostrar_Expediciones_Aux (Lista_Expediciones.First);
      end if;
   end Mostrar_Expediciones;

   procedure Mostrar_Expediciones_Aux (Expedición : Expedición'Access) is
   begin
      loop
         Ada.Text_IO.Put ("Nombre: ");
         Ada.Text_IO.Put_Line (Expedición.All.Nombre);

         Ada.Text_IO.Put ("Nave espacial: ");
         Ada.Text_IO.Put_Line (Expedición.All.Nave_Espacial);

         Ada.Text_IO.Put ("Misión: ");
         Ada.Text_IO.Put_Line (Expedición.All.Mision);

         Ada.Text_IO.Put ("Planeta destino: ");
         Ada.Text_IO.Put_Line (Expedición.All.Destino);

         Ada.Text_IO.New_Line;

         if Expedition.Next'Access = null then
            exit;
         else
            Mostrar_Expediciones_Aux (Expedition.Next'Access);
         end if;
      end loop;
   end Mostrar_Expediciones_Aux;

begin
   Mostrar_Planetas (Planeta'First);
   Ada.Text_IO.New_Line;

   Mostrar_Naves_Espaciales (Nave_Espacial'First);
   Ada.Text_IO.New_Line;

   Crear_Expedición;
   Ada.Text_IO.New_Line;

   Mostrar_Expediciones;
end Ciencia_Ficción;
```

Descripción del código:

* Se definen tres tipos enumerados: `Planeta`, `Nave_Espacial` y `Mision`. Estos tipos representan los planetas, las naves espaciales y las misiones que se pueden llevar a cabo en las expediciones.
* Se definen dos procedimientos, `Mostrar_Planetas` y `Mostrar_Naves_Espaciales`, que muestran por pantalla los elementos de las listas de planetas y naves espaciales, respectivamente.
* Se define un tipo de registro, `Expedición`, que almacena la información de una expedición espacial, incluyendo el nombre, el destino, la nave espacial y la misión.
* Se define un tipo de lista, `Lista_Expediciones`, que se utiliza para almacenar las expediciones espaciales.
* Se define un procedimiento, `Crear_Expedición`, que permite al usuario crear una nueva expedición espacial.
* Se define un procedimiento, `Mostrar_Expediciones`, que muestra por pantalla las expediciones espaciales almacenadas en la lista.
* Se define un procedimiento auxiliar, `Mostrar_Expediciones_Aux`, que se utiliza para mostrar por pantalla las expediciones espaciales almacenadas en una lista, de forma recursiva.

El procedimiento principal del programa, `Ciencia_Ficción`, llama a los procedimientos `Mostrar_Planetas`, `Mostrar_Naves_Espaciales`, `Crear_Expedición` y `Mostrar_Expediciones` para mostrar por pantalla los planetas, las naves espaciales, las expediciones espaciales y las expediciones espaciales creadas por el usuario, respectivamente.