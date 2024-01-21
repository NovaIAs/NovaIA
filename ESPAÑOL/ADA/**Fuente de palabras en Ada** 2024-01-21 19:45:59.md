```ada
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

procedure FuenteDePalabras is

   type ColaDePalabras is limited string(1..20);

   Cola : ColaDePalabras;

begin
   Cola := "perro, gato, lluvia, sol, mesa, silla, libro, lapiz, papel, tijera";
   Put(Cola);
end FuenteDePalabras;
```

Este código en Ada crea un tipo de dato llamado `ColaDePalabras` que es una cadena limitada de caracteres de 1 a 20 caracteres. Luego, declara una variable `Cola` de tipo `ColaDePalabras` y la inicializa con una cadena de palabras separadas por comas. Finalmente, llama a la función `Put` para imprimir la cadena.

El siguiente código es una versión más compleja del anterior que utiliza un bucle `for` para recorrer la cadena de palabras y buscar una palabra específica:

```ada
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

procedure FuenteDePalabras is

   type ColaDePalabras is limited string(1..20);

   Cola : ColaDePalabras;

begin
   Cola := "perro, gato, lluvia, sol, mesa, silla, libro, lapiz, papel, tijera";
   for I in Cola'Range loop
      if Cola(I) = "silla" then
         Put_Line(Cola(I));
      end if;
   end loop;
end FuenteDePalabras;
```

Este código es más complejo porque utiliza un bucle `for` para recorrer la cadena de palabras y buscar la palabra "silla". Si encuentra la palabra, la imprime utilizando la función `Put_Line`.

El siguiente código es una versión aún más compleja del anterior que utiliza un procedimiento recursivo para recorrer la cadena de palabras y buscar una palabra específica:

```ada
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

procedure FuenteDePalabras is

   type ColaDePalabras is limited string(1..20);

   Cola : ColaDePalabras;

procedure BuscarPalabra(Cadena : ColaDePalabras; Palabra : ColaDePalabras) is
begin
   if Cadena(1) = Palabra then
      Put_Line(Cadena(1));
   elsif Cadena'Length > 1 then
      BuscarPalabra(Cadena(2..Cadena'Last), Palabra);
   end if;
end BuscarPalabra;

begin
   Cola := "perro, gato, lluvia, sol, mesa, silla, libro, lapiz, papel, tijera";
   BuscarPalabra(Cola, "silla");
end FuenteDePalabras;
```

Este código es más complejo porque utiliza un procedimiento recursivo llamado `BuscarPalabra` para recorrer la cadena de palabras y buscar la palabra "silla". El procedimiento `BuscarPalabra` recibe dos parámetros: la cadena de palabras y la palabra que se quiere buscar. El procedimiento recorre la cadena de palabras de forma recursiva hasta encontrar la palabra que se busca o hasta que llega al final de la cadena.

El siguiente código es una versión aún más compleja del anterior que utiliza un procedimiento genérico para recorrer la cadena de palabras y buscar una palabra específica:

```ada
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

procedure FuenteDePalabras is

   type ColaDePalabras is limited string(1..20);

   Cola : ColaDePalabras;

procedure BuscarPalabra[T : String] (Cadena : ColaDePalabras) is
begin
   for I in Cadena'Range loop
      if Cadena(I) = T then
         Put_Line(Cadena(I));
      end if;
   end loop;
end BuscarPalabra;

begin
   Cola := "perro, gato, lluvia, sol, mesa, silla, libro, lapiz, papel, tijera";
   BuscarPalabra("silla")(Cola);
end FuenteDePalabras;
```

Este código es más complejo porque utiliza un procedimiento genérico llamado `BuscarPalabra` para recorrer la cadena de palabras y buscar una palabra específica. El procedimiento genérico `BuscarPalabra` recibe un parámetro de tipo `String` que es la palabra que se quiere buscar. El procedimiento recorre la cadena de palabras de forma iterativa hasta encontrar la palabra que se busca o hasta que llega al final de la cadena.