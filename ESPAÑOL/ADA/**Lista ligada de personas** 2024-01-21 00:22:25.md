```ada
with Ada.Strings.Text_IO; use Ada.Strings.Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

procedure Complejo is
   -- Tipos de datos y estructuras

   -- Registro con los datos de una persona
   type Persona is record
      Nombre : String(1..50);
      Edad   : Integer;
   end record;

   -- Lista ligada de personas
   type Nodo is record
      Persona : Persona;
      Siguiente : Nodo'Access;
   end record;

   -- Variables globales

   -- Lista de personas
   Personas : Nodo'Access := null;

   -- Procedimientos y funciones

   -- Crear una nueva persona
   function CreaPersona(Nombre : String; Edad : Integer) return Persona is
   begin
      return (Persona'(Nombre, Edad));
   end CreaPersona;

   -- Añadir una persona a la lista
   procedure AñadePersona(Persona : Persona) is
   begin
      if Personas is null then
         Personas := new Nodo'(Persona, null);
      else
         Personas.Siguiente := new Nodo'(Persona, Personas);
      end if;
   end AñadePersona;

   -- Mostrar una persona
   procedure MuestraPersona(Persona : Persona) is
   begin
      Put_Line(Persona.Nombre & " (" & Persona.Edad & " años)");
   end MuestraPersona;

   -- Mostrar todas las personas
   procedure MuestraPersonas is
   begin
      for Persona in Personas loop
         MuestraPersona(Persona.Persona);
      end loop;
   end MuestraPersonas;

   -- Procedimiento principal

begin
   -- Añadir algunas personas a la lista
   AñadePersona(CreaPersona("Juan", 20));
   AñadePersona(CreaPersona("María", 25));
   AñadePersona(CreaPersona("Pedro", 30));

   -- Mostrar todas las personas
   Put_Line("Lista de personas:");
   MuestraPersonas;

   -- Finalizar la ejecución
end Complejo;
```

**Explicación del código:**

* El código define un tipo de dato `Persona` que contiene dos campos: `Nombre` y `Edad`.
* También define un tipo de dato `Nodo` que representa un nodo de una lista ligada.
* La variable global `Personas` es una lista ligada de personas.
* El código define tres procedimientos: `CreaPersona`, `AñadePersona` y `MuestraPersonas`.
* El procedimiento `CreaPersona` crea una nueva persona.
* El procedimiento `AñadePersona` añade una persona a la lista de personas.
* El procedimiento `MuestraPersonas` muestra todas las personas de la lista.
* El procedimiento principal crea algunas personas y las añade a la lista, y luego muestra la lista.