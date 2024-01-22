```ada
-- Procedimiento principal (punto de entrada del programa)
with Ada.Text_IO;

procedure Multiuso is
   type Tipo_Registro is
   record
      Nombre: String(1..100);
      Edad: Integer;
      Altura: Float;
   end record;

   -- Crear una variable de tipo Tipo_Registro
   Persona: Tipo_Registro;

   -- Solicitar datos al usuario e ingresarlos en la variable Persona
   begin
      Ada.Text_IO.Put("Ingrese su nombre: ");
      Ada.Text_IO.Get_Line(Persona.Nombre);

      Ada.Text_IO.Put("Ingrese su edad: ");
      Ada.Text_IO.Get(Persona.Edad);

      Ada.Text_IO.Put("Ingrese su altura (en metros): ");
      Ada.Text_IO.Get(Persona.Altura);
   end Multiuso;
```