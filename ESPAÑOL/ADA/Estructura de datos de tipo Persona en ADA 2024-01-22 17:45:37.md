```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Persona is

   type Nombre is array (1..20) of Character;

   type Direccion is record
      Calle : Nombre;
      Ciudad : Nombre;
      CP : Integer;
   end record;

   type Fecha is record
      Dia : Integer;
      Mes : Integer;
      Anio : Integer;
   end record;

   type Persona is record
      Nombre : Nombre;
      Direccion : Direccion;
      Fecha_Nacimiento : Fecha;
   end record;

   P : Persona; -- Una variable de tipo Persona

begin
   P.Nombre := "Juan García";
   P.Direccion.Calle := "Calle Mayor, 123";
   P.Direccion.Ciudad := "Madrid";
   P.Direccion.CP := 28013;
   P.Fecha_Nacimiento.Dia := 15;
   P.Fecha_Nacimiento.Mes := 3;
   P.Fecha_Nacimiento.Anio := 1980;

   Put_Line("Nombre: " & P.Nombre);
   Put_Line("Dirección:");
   Put_Line("Calle: " & P.Direccion.Calle);
   Put_Line("Ciudad: " & P.Direccion.Ciudad);
   Put_Line("CP: " & Integer'Image(P.Direccion.CP));
   Put_Line("Fecha de nacimiento:");
   Put_Line("Día: " & Integer'Image(P.Fecha_Nacimiento.Dia));
   Put_Line("Mes: " & Integer'Image(P.Fecha_Nacimiento.Mes));
   Put_Line("Año: " & Integer'Image(P.Fecha_Nacimiento.Anio));
end Persona;
```

Este código crea un tipo de datos llamado `Persona` que contiene un nombre, una dirección y una fecha de nacimiento. Luego, crea una variable de tipo `Persona` llamada `P` y le asigna valores a sus campos. Finalmente, imprime los valores de los campos de `P` en la consola.

El código está dividido en tres partes principales:

* **La definición del tipo de datos `Persona`:** Esta parte del código define el tipo de datos `Persona` y sus campos. Los campos son: `Nombre` (un array de 20 caracteres), `Direccion` (un registro que contiene una calle, una ciudad y un código postal) y `Fecha_Nacimiento` (un registro que contiene un día, un mes y un año).
* **La creación de la variable `P`:** Esta parte del código crea una variable de tipo `Persona` llamada `P`.
* **La asignación de valores a los campos de `P`:** Esta parte del código asigna valores a los campos de `P`. Los valores son: `Nombre` = "Juan García", `Direccion.Calle` = "Calle Mayor, 123", `Direccion.Ciudad` = "Madrid", `Direccion.CP` = 28013, `Fecha_Nacimiento.Dia` = 15, `Fecha_Nacimiento.Mes` = 3 y `Fecha_Nacimiento.Anio` = 1980.
* **La impresión de los valores de los campos de `P`:** Esta parte del código imprime los valores de los campos de `P` en la consola.

El código utiliza varias características del lenguaje Ada, como los tipos de datos definidos por el usuario, los registros, las variables y la impresión de datos.