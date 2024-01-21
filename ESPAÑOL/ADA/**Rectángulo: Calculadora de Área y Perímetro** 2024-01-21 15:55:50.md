```ada
with Ada.Text_IO;

procedure Rectangulo is
   type Punto is record
      X, Y : Float;
   end record;

   type Rectangulo is record
      P1, P2 : Punto;
   end record;

   function Area(R : Rectangulo) return Float is
   begin
      return Abs((R.P2.X - R.P1.X) * (R.P2.Y - R.P1.Y));
   end Area;

   function Perimetro(R : Rectangulo) return Float is
   begin
      return 2 * (Abs(R.P2.X - R.P1.X) + Abs(R.P2.Y - R.P1.Y));
   end Perimetro;

begin
   declare
      R : Rectangulo;
   begin
      Ada.Text_IO.Put("Ingrese el punto P1 (X, Y): ");
      Ada.Text_IO.Get(R.P1.X);
      Ada.Text_IO.Get(R.P1.Y);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("Ingrese el punto P2 (X, Y): ");
      Ada.Text_IO.Get(R.P2.X);
      Ada.Text_IO.Get(R.P2.Y);
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put("El área del rectángulo es: ");
      Ada.Text_IO.Put(Area(R), 2, 1);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("El perímetro del rectángulo es: ");
      Ada.Text_IO.Put(Perimetro(R), 2, 1);
      Ada.Text_IO.New_Line;
   end;
end Rectangulo;
```

Explicación del código:

* Se define un tipo de dato llamado `Punto` que representa un punto en el plano cartesiano. El tipo `Punto` tiene dos campos, `X` e `Y`, que representan las coordenadas X e Y del punto.
* Se define un tipo de dato llamado `Rectangulo` que representa un rectángulo en el plano cartesiano. El tipo `Rectangulo` tiene dos campos, `P1` y `P2`, que representan los dos puntos que definen el rectángulo.
* Se definen dos funciones, `Area` y `Perimetro`, que calculan el área y el perímetro de un rectángulo, respectivamente.
* Se declara una variable `R` de tipo `Rectangulo`.
* Se solicita al usuario que introduzca los dos puntos que definen el rectángulo.
* Se calculan el área y el perímetro del rectángulo utilizando las funciones `Area` y `Perimetro`, respectivamente.
* Se muestran el área y el perímetro del rectángulo en la consola.