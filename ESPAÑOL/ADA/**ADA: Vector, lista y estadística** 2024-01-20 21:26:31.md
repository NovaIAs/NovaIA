```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Strings.Unbounded;

procedure Main is
   type Rango is range 1..100;

   type Vector is array(Rango) of Integer;
   V: Vector;

   type Nodo is record
      Dato: Integer;
      Sig: access Nodo;   -- Accede al siguiente nodo en la lista
   end record;

   N: access Nodo;

   function Media(Vec: Vector) return Float is
      Suma: Integer := 0;
      for I in Vec'First..Vec'Last loop
         Suma := Suma + Vec(I);
      end loop;
      return Float(Suma) / Float(Vec'Last - Vec'First + 1);
   end Media;

   procedure Inicia_Lista(L: in out access Nodo) is
      L := new Nodo(Dato => 1, Sig => null);
      for I in 2..10 loop
         L.Sig := new Nodo(Dato => I, Sig => null);
         L := L.Sig;  -- Avanzamos a la siguiente celda
      end loop;
   end Inicia_Lista;

   procedure Escribe_Lista(L: in access Nodo) is
      while L /= null loop
         Ada.Text_IO.Put(L.Dato);
         if L.Sig /= null then Ada.Text_IO.Put(", "); end if;
         L := L.Sig;
      end loop;
   end Escribe_Lista;

begin
   -- Llenamos el vector con valores al azar
   for I in V'First..V'Last loop
      V(I) := Random(100); -- Generamos un número aleatorio entre 0 y 99
   end loop;

   -- Imprimimos el vector
   Ada.Text_IO.Put_Line("Vector:");
   for I in V'First..V'Last loop
      Ada.Integer_Text_IO.Put(V(I));
      if I /= V'Last then Ada.Text_IO.Put(", "); end if;
   end loop;

   -- Calculamos la media del vector
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line("Media del vector: ");
   Ada.Float_Text_IO.Put(Media(V));

   -- Creamos la lista de nodos
   Inicia_Lista(N);

   -- Imprimimos la lista
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line("Lista:");
   Escribe_Lista(N);

   Ada.Text_IO.New_Line;
end Main;
```

Este código complejo en Ada:

- Define un tipo de rango llamado `Rango` con valores entre 1 y 100.
- Define un tipo de datos llamado `Vector` que es un array de enteros de rango `Rango`.
- Define un tipo de datos llamado `Nodo` que es un registro que contiene un campo de datos entero llamado `Dato` y un campo de referencia a otro nodo llamado `Sig`.
- Define una función llamada `Media` que calcula la media de un vector de enteros.
- Define un procedimiento llamado `Inicia_Lista` que crea una lista de nodos enlazados con los valores de `1` a `10`.
- Define un procedimiento llamado `Escribe_Lista` que recorre la lista y muestra los valores de los nodos por pantalla.
- En la función `main`, se crea un vector `V` de enteros y se llena con valores aleatorios entre 0 y 99.
- Se calcula la media del vector y se muestra por pantalla.
- Se crea una lista de nodos `N` usando el procedimiento `Inicia_Lista`.
- Se muestra la lista de nodos por pantalla utilizando el procedimiento `Escribe_Lista`.