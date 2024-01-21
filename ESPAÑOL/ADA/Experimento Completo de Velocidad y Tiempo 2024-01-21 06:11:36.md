```ada
with Ada.Text_IO, Ada.Real_Time, Ada.Integer_Text_IO;

procedure Experimento_Completo is

   type Velocidad is new Float;
   type Temps is new Float;

   procedure Mostrar_Velocidad (Titulo : String; Velocidad : in Velocidad) is
   begin
      Ada.Text_IO.Put_Line (Titulo & " = " & Float'Image (Velocidad));
   end Mostrar_Velocidad;

   procedure Mostrar_Tiempo (Titulo : String; Tiempo : in Temps) is
   begin
      Ada.Text_IO.Put_Line (Titulo & " = " & Float'Image (Tiempo));
   end Mostrar_Tiempo;

   function Calcular_Velocidad (Distancia : in Float; Tiempo : in Temps) return Velocidad is
   begin
      return Distancia / Tiempo;
   end Calcular_Velocidad;

   function Calcular_Tiempo (Distancia : in Float; Velocidad : in Velocidad) return Temps is
   begin
      return Distancia / Velocidad;
   end Calcular_Tiempo;

begin

   declare
      Distancia : Float := 1000.0;
      Tiempo : Temps := 100.0;
      Velocidad : Velocidad;
   begin
      Mostrar_Velocidad ("Velocidad Inicial", Calcular_Velocidad (Distancia, Tiempo));
      Mostrar_Tiempo ("Tiempo Inicial", Tiempo);
      Velocidad := Ada.Integer_Text_IO.Get_Float ("Introduce la nueva velocidad: ");
      Tiempo := Calcular_Tiempo (Distancia, Velocidad);
      Mostrar_Velocidad ("Nueva Velocidad", Velocidad);
      Mostrar_Tiempo ("Nuevo Tiempo", Tiempo);
   end;

end Experimento_Completo;
```

Este código implementa un pequeño experimento que calcula la velocidad de un objeto en movimiento dado una distancia y un tiempo iniciales, y luego permite al usuario introducir una nueva velocidad y calcula el nuevo tiempo que tardará el objeto en recorrer la misma distancia.

El código está dividido en varios módulos:

* El procedimiento `Mostrar_Velocidad` muestra por pantalla el valor de una velocidad dada.
* El procedimiento `Mostrar_Tiempo` muestra por pantalla el valor de un tiempo dado.
* La función `Calcular_Velocidad` calcula la velocidad de un objeto en movimiento dada una distancia y un tiempo.
* La función `Calcular_Tiempo` calcula el tiempo que tarda un objeto en movimiento en recorrer una distancia dada a una velocidad dada.
* El procedimiento `Experimento_Completo` es el punto de entrada del programa. Declara las variables necesarias y llama a las funciones y procedimientos anteriores para realizar el experimento.

El código utiliza el paquete `Ada.Text_IO` para la entrada y salida de datos por pantalla, el paquete `Ada.Real_Time` para obtener el tiempo actual y el paquete `Ada.Integer_Text_IO` para la entrada y salida de datos numéricos.