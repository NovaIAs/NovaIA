```ada
with Ada.Text_IO; use Ada.Text_IO;
with Data_Structures.Multi_Set; use Data_Structures.Multi_Set;
with Data_Structures.Hash_Multimap; use Data_Structures.Hash_Multimap;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

procedure Complejo is

   type Mensaje is access String;
   type Conjunto_Mensajes is new Multi_Set (Mensaje);

   type Mapa_Mensajes is new Hash_Multimap (String, Conjunto_Mensajes);

   type Persona es record
      Nombre: String (1..20);
      Mensajes: Mapa_Mensajes;
   end record;

   type Conjunto_Personas is new Multi_Set (Persona);

   type Red_Social is record
      Personas: Conjunto_Personas;
   end record;

   procedure Enviar (Remitente: Persona; Destinatario: Persona; Mensaje: String) is
   begin
      Remitente.Mensajes (Destinatario.Nombre).Insert (Mensaje);
   end Enviar;

   procedure Listar_Mensajes (Persona: Persona) is
   begin
      for Mensaje in Persona.Mensajes loop
         Put (Mensaje.Item);
         New_Line;
      end loop;
   end Listar_Mensajes;

   JR: Persona := (Nombre => "Juan Rodríguez", Mensajes => new Mapa_Mensajes);
   MR: Persona := (Nombre => "María Ramírez", Mensajes => new Mapa_Mensajes);
   LS: Persona := (Nombre => "Luis Sánchez", Mensajes => new Mapa_Mensajes);

   Red: Red_Social := (Personas => new Conjunto_Personas);

   Red.Personas.Insert (JR);
   Red.Personas.Insert (MR);
   Red.Personas.Insert (LS);

   Enviar (JR, MR, "Hola, María!");
   Enviar (MR, JR, "Hola, Juan! ¿Cómo estás?");
   Enviar (LS, JR, "Hola, Juan! ¿Qué tal?");
   Enviar (JR, LS, "Hola, Luis! Bien, gracias.");

   Listar_Mensajes (JR);
   Listar_Mensajes (MR);
   Listar_Mensajes (LS);

begin
   null;
end Complejo;
```

Explicación del código:

* El código define varios tipos de datos:
    * `Mensaje` es un tipo de acceso a una cadena de caracteres.
    * `Conjunto_Mensajes` es un tipo de conjunto múltiple de mensajes.
    * `Mapa_Mensajes` es un tipo de mapa múltiple de cadenas de caracteres a conjuntos de mensajes.
    * `Persona` es un tipo de registro que representa a una persona en la red social. Cada persona tiene un nombre y un mapa de mensajes.
    * `Conjunto_Personas` es un tipo de conjunto múltiple de personas.
    * `Red_Social` es un tipo de registro que representa a la red social. Contiene un conjunto de personas.

* El código también define varios procedimientos:
    * `Enviar` envía un mensaje de una persona a otra.
    * `Listar_Mensajes` enumera los mensajes recibidos por una persona.

* El código crea tres instancias de `Persona`: `JR`, `MR` y `LS`. También crea una instancia de `Red_Social` llamada `Red`.

* El código agrega las tres personas a la red social.

* El código envía algunos mensajes entre las personas.

* El código enumera los mensajes recibidos por cada persona.