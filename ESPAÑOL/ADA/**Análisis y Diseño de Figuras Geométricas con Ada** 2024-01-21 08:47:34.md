```ada
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding;
with Ada.Characters.Latin_1;

procedure Complejo is
   type Punto is record
      X, Y: Integer;
   end record;

   type Linea is record
      Origen, Destino: Punto;
   end record;

   type Circulo is record
      Centro: Punto;
      Radio: Integer;
   end record;

   type Figura is discriminated record
      case Tipo: Figura_Tipo is
         Punto => (Punto);
         Linea => (Linea);
         Circulo => (Circulo);
      end case;
   end record;

   function Distancia(P1, P2: Punto) return Integer is
   begin
      return Integer'(P1.X - P2.X)**2 + Integer'(P1.Y - P2.Y)**2;
   end Distancia;

   procedure Dibujar(F: Figura) is
   begin
      case F.Tipo is
         when Punto =>
            Ada.Text_IO.Put("Punto (" & Integer'Image(F.Punto.X) & "," &
                              Integer'Image(F.Punto.Y) & ")");
         when Linea =>
            Ada.Text_IO.Put("Linea desde (" & Integer'Image(F.Linea.Origen.X) & "," &
                              Integer'Image(F.Linea.Origen.Y) & ") hasta (" &
                              Integer'Image(F.Linea.Destino.X) & "," &
                              Integer'Image(F.Linea.Destino.Y) & ")");
         when Circulo =>
            Ada.Text_IO.Put("Circulo con centro en (" &
                              Integer'Image(F.Circulo.Centro.X) & "," &
                              Integer'Image(F.Circulo.Centro.Y) &
                              ") y radio " & Integer'Image(F.Circulo.Radio));
         when others =>
            Ada.Text_IO.Put("Figura desconocida");
      end case;
   end Dibujar;

begin
   declare
      P1: Punto := (10, 20);
      P2: Punto := (30, 40);
      L1: Linea := (P1, P2);
      C1: Circulo := (P1, 15);
      Figuras: array (1..3) of Figura := (P1, L1, C1);
   begin
      for I in Figuras'Range loop
         Dibujar(Figuras(I));
         Ada.Text_IO.New_Line;
      end loop;

      Ada.Text_IO.Put("La distancia entre (" & Integer'Image(P1.X) & "," &
                        Integer'Image(P1.Y) & ") y (" & Integer'Image(P2.X) & "," &
                        Integer'Image(P2.Y) & ") es " & Integer'Image(Distancia(P1, P2)));
   end;
end Complejo;
```

Explicación:

* El código define tres tipos de figuras: punto, línea y círculo.
* La función `Distancia` calcula la distancia entre dos puntos.
* El procedimiento `Dibujar` dibuja una figura en la pantalla.
* El código principal crea tres figuras y las dibuja en la pantalla.
* También calcula la distancia entre dos puntos.