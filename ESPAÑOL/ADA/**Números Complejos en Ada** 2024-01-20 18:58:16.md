```ada
with Generics, Ada.Streams.Stream_IO;
procedure Ada_Complexa is
   package Complejos is new Generics(Float);
   use Complejos;
   Lista: array (1..10) of Complejo; -- Arreglo de Números Complejos
begin
   for I in 1..10 loop
      Lista(I) := Complejo'(I, I*2); -- Crear números complejos
   end loop;

   Ada.Streams.Stream_IO.Put_Line("Números complejos:");
   for I in 1..10 loop
      Ada.Streams.Stream_IO.Put(Lista(I).Real);
      Ada.Streams.Stream_IO.Put(" + ");
      Ada.Streams.Stream_IO.Put(Lista(I).Imaginario);
      Ada.Streams.Stream_IO.Put("i\n");
   end loop;
end Ada_Complexa;
```

Explicación del código:

1. El código crea un nuevo tipo llamado `Complejo` usando la utilidad `Generics` para crear tipos parametrizados. Este tipo representa números complejos con partes reales e imaginarias.

2. La variable `Lista` es un arreglo de 10 elementos de tipo Complejo.

3. El bucle `for` inicializa la variable `Lista` con números complejos. Cada elemento se crea usando el constructor de tipos `Complejo'`, que toma dos argumentos: la parte real y la parte imaginaria.

4. El código utiliza el paquete `Ada.Streams.Stream_IO` para mostrar en la consola los valores de la variable `Lista`. El bucle `for` recorre cada elemento de `Lista` y utiliza las funciones `Put_Line`, `Put` y `Put_Line` para mostrar la parte real, el signo `+` (ya que son números complejos), la parte imaginaria y un salto de línea.