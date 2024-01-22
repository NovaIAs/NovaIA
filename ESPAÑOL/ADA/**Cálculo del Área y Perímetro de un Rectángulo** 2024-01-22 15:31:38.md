```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Command_Line;

procedure Complejo is
   type Punto is record
      x, y : Float;
   end record;

   type Linea is record
      p1, p2 : Punto;
   end record;

   type Rectangulo is record
      p1, p2, p3, p4 : Punto;
   end record;

   function Area(r : Rectangulo) return Float is
      b1 : Float := r.p1.x - r.p2.x;
      b2 : Float := r.p2.y - r.p3.y;
      return b1 * b2;
   end Area;

   function Perimetro(r : Rectangulo) return Float is
      b1 : Float := r.p1.x - r.p2.x;
      b2 : Float := r.p2.y - r.p3.y;
      return 2 * (b1 + b2);
   end Perimetro;

   procedure Mostrar(r : Rectangulo) is
   begin
      Ada.Text_IO.Put_Line("Rectangulo:");
      Ada.Text_IO.Put("Punto 1: ");
      Ada.Float_Text_IO.Put(r.p1.x, 1, 0);
      Ada.Text_IO.Put(", ");
      Ada.Float_Text_IO.Put(r.p1.y, 1, 0);
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put("Punto 2: ");
      Ada.Float_Text_IO.Put(r.p2.x, 1, 0);
      Ada.Text_IO.Put(", ");
      Ada.Float_Text_IO.Put(r.p2.y, 1, 0);
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put("Punto 3: ");
      Ada.Float_Text_IO.Put(r.p3.x, 1, 0);
      Ada.Text_IO.Put(", ");
      Ada.Float_Text_IO.Put(r.p3.y, 1, 0);
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put("Punto 4: ");
      Ada.Float_Text_IO.Put(r.p4.x, 1, 0);
      Ada.Text_IO.Put(", ");
      Ada.Float_Text_IO.Put(r.p4.y, 1, 0);
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put("Area: ");
      Ada.Float_Text_IO.Put(Area(r), 1, 0);
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put("Perimetro: ");
      Ada.Float_Text_IO.Put(Perimetro(r), 1, 0);
      Ada.Text_IO.New_Line;
   end Mostrar;

begin
   declare
      r : Rectangulo := (
         (0.0, 0.0),
         (10.0, 0.0),
         (10.0, 10.0),
         (0.0, 10.0)
      );
   begin
      Mostrar(r);
   end;
end Complejo;
```

El código anterior hace lo siguiente:

1. Define un tipo de datos llamado `Punto` que tiene dos campos, `x` e `y`, ambos de tipo `Float`.
2. Define un tipo de datos llamado `Linea` que tiene dos campos, `p1` y `p2`, ambos de tipo `Punto`.
3. Define un tipo de datos llamado `Rectangulo` que tiene cuatro campos, `p1`, `p2`, `p3` y `p4`, todos de tipo `Punto`.
4. Define una función llamada `Area` que toma un argumento de tipo `Rectangulo` y devuelve un valor de tipo `Float`. Esta función calcula el área del rectángulo.
5. Define una función llamada `Perimetro` que toma un argumento de tipo `Rectangulo` y devuelve un valor de tipo `Float`. Esta función calcula el perímetro del rectángulo.
6. Define un procedimiento llamado `Mostrar` que toma un argumento de tipo `Rectangulo` y lo muestra en la consola.
7. Declara una variable llamada `r` de tipo `Rectangulo` y la inicializa con los valores especificados.
8. Llama al procedimiento `Mostrar` con el argumento `r` para mostrar el rectángulo en la consola.

El código está escrito en el lenguaje de programación Ada y utiliza las siguientes características del lenguaje:

* Tipos de datos definidos por el usuario
* Funciones y procedimientos
* Variables
* Declaraciones de constantes
* Expresiones aritméticas y lógicas
* Sentencias de control
* Entrada y salida de datos