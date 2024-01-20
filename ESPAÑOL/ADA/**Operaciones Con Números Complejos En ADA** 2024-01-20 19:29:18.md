```ada
with Ada.Text_IO;              -- para manejar la entrada y la salida de texto
with Ada.Integer_Text_IO;     -- para manejar la conversión de enteros a texto
with Ada.Numerics           -- para manejar operaciones aritméticas

procedure Complejo is

   type Complejo is record
      x : Float;
      y : Float;
   end record;

   function "+" (izq, der: Complejo) return Complejo is
   begin
      return (izq.x + der.x, izq.y + der.y);
   end "+";

   function "-" (izq, der: Complejo) return Complejo is
   begin
      return (izq.x - der.x, izq.y - der.y);
   end "-";

   function "*" (izq, der: Complejo) return Complejo is
   begin
      return (izq.x * der.x - izq.y * der.y, izq.x * der.y + izq.y * der.x);
   end "*";

   function "/" (izq, der: Complejo) return Complejo is
   begin
      if der.x = 0.0 and der.y = 0.0 then
         raise Constraint_Error;      -- división por cero
      end if;
      return ((izq.x * der.x + izq.y * der.y) / (der.x * der.x + der.y * der.y),
              (izq.y * der.x - izq.x * der.y) / (der.x * der.x + der.y * der.y));
   end "/";

   function Módulo (n: Complejo) return Float is
   begin
      return Sqrt (n.x * n.x + n.y * n.y);
   end Módulo;

   function Argumento (n: Complejo) return Float is
   begin
      return Atan2 (n.y, n.x);
   end Argumento;

   procedure MostrarComplejo (c: Complejo) is
   begin
      Ada.Text_IO.Put (Float'Image (c.x));
      Ada.Text_IO.Put (" + ");
      Ada.Text_IO.Put (Float'Image (c.y));
      Ada.Text_IO.Put ("i");
   end MostrarComplejo;

begin
   declare
      z1, z2, z3: Complejo;
   begin
      z1 := Complejo (3.0, 4.0);
      z2 := Complejo (5.0, 6.0);
      Ada.Text_IO.Put_Line ("z1 = ");
      MostrarComplejo (z1);
      Ada.Text_IO.Put_Line ("z2 = ");
      MostrarComplejo (z2);
      z3 := z1 + z2;
      Ada.Text_IO.Put_Line ("z1 + z2 = ");
      MostrarComplejo (z3);
      z3 := z1 - z2;
      Ada.Text_IO.Put_Line ("z1 - z2 = ");
      MostrarComplejo (z3);
      z3 := z1 * z2;
      Ada.Text_IO.Put_Line ("z1 * z2 = ");
      MostrarComplejo (z3);
      z3 := z1 / z2;
      Ada.Text_IO.Put_Line ("z1 / z2 = ");
      MostrarComplejo (z3);
      Ada.Text_IO.Put_Line ("Módulo de z1 = ");
      Ada.Integer_Text_IO.Put (Módulo (z1));
      Ada.Text_IO.Put_Line ("Argumento de z1 = ");
      Ada.Integer_Text_IO.Put (Argumento (z1));
   end;
end Complejo;
```

Explicación del código:

* El tipo `Complejo` es un registro que contiene dos componentes, `x` e `y`, que representan las partes real e imaginaria de un número complejo.
* Las funciones "+", "-", "*", y "/" implementan las operaciones de suma, resta, multiplicación y división de números complejos.
* La función `Módulo` calcula el módulo de un número complejo, que es la raíz cuadrada de la suma de las partes real e imaginaria al cuadrado.
* La función `Argumento` calcula el argumento de un número complejo, que es el ángulo entre el eje real y el vector que representa al número complejo en el plano complejo.
* El procedimiento `MostrarComplejo` muestra un número complejo en la consola.
* El procedimiento `Complejo` es el programa principal que crea dos números complejos, `z1` y `z2`, y realiza una serie de operaciones con ellos, mostrando los resultados en la consola.