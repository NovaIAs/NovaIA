```ada
with Ada.Text_IO;

procedure Complejo is
   type Lista_Complejos is array (Positive range <>) of Complex;

   type Complejo is record
      real : Float;
      imag : Float;
   end record;

   Lista : Lista_Complejos;

   function Suma (Arg : Lista_Complejos) return Complejo is
   begin
      if Arg'Length = 0 then
         return (0.0, 0.0);
      else
         return Arg(Arg'First) + Suma (Arg (Arg'First + 1 .. Arg'Last));
      end if;
   end Suma;

   function Producto (Arg : Lista_Complejos) return Complejo is
   begin
      if Arg'Length = 0 then
         return (0.0, 0.0);
      else
         return Arg(Arg'First) * Producto (Arg (Arg'First + 1 .. Arg'Last));
      end if;
   end Producto;

   procedure Mostrar (Arg : Lista_Complejos) is
   begin
      for I in Arg'Range loop
         Ada.Text_IO.Put ("(");
         Ada.Text_IO.Put (Arg(I).real, 2, 4);
         Ada.Text_IO.Put (", ");
         Ada.Text_IO.Put (Arg(I).imag, 2, 4);
         Ada.Text_IO.Put (") ");
      end loop;
      Ada.Text_IO.New_Line;
   end Mostrar;

begin
   Lista := ((1.0, 2.0), (3.0, 4.0), (5.0, 6.0));

   Mostrar (Lista);
   Ada.Text_IO.Put ("Suma de los complejos: ");
   Mostrar (Suma (Lista));
   Ada.Text_IO.Put ("Producto de los complejos: ");
   Mostrar (Producto (Lista));
end Complejo;
```

Explicación:

* El código define un tipo de datos complejo, que consta de dos componentes: la parte real y la parte imaginaria.
* También define una lista de complejos, que es una matriz de complejos.
* Hay dos funciones, `Suma` y `Producto`, que calculan la suma y el producto de una lista de complejos, respectivamente.
* Hay un procedimiento, `Mostrar`, que imprime una lista de complejos en la consola.
* En el `main` del programa, se crea una lista de complejos y se muestran sus elementos.
* Se llama a las funciones `Suma` y `Producto` para calcular la suma y el producto de la lista, y los resultados se muestran en la consola.