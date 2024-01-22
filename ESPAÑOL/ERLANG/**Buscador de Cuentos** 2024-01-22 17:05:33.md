```erlang
-module(baúl_de_cuentos).
-export([contar_cuento/1]).

contar_cuento(Titulo) ->
  Cuentos = [{cuento:"El patito feo", autor:"Hans Christian Andersen", público:"niños"},
             {cuento:"La Cenicienta", autor:"Charles Perrault", público:"niños"},
             {cuento:"La bella y la bestia", autor:"Jean Cocteau", público:"adultos"},
             {cuento:"El señor de los anillos", autor:"J.R.R. Tolkien", público:"adultos"}],
  CuentosFiltrados = [C || C <- Cuentos, C#cuento.cuento == Titulo],
  case CuentosFiltrados of
    [] -> io:format("Lo siento, no conozco ningún cuento con el título ~p.~n", [Titulo]);
    [{cuento:Cuento, autor:Autor, público:Público}] ->
      io:format("El cuento ~p fue escrito por ~p y está dirigido a un público ~p.~n", [Cuento, Autor, Público])
  end.
```

Este código es una función que recibe un título de cuento y devuelve información sobre el cuento, como su autor, su público objetivo y su trama (si se desea incluir esta última). La función utiliza una lista de tuplas para almacenar los datos de los cuentos, y la función `[C || C <- Cuentos, C#cuento.cuento == Titulo]` filtra la lista de tuplas para devolver sólo el cuento que coincida con el título dado.

Si no se encuentra ningún cuento con el título dado, la función imprime un mensaje de error. Si se encuentra un cuento, la función imprime el título, el autor, el público objetivo y la trama del cuento.

El código utiliza la notación de comprensión de listas para filtrar la lista de tuplas. La expresión `[C || C <- Cuentos, C#cuento.cuento == Titulo]` es equivalente a la siguiente expresión:

```erlang
CuentosFiltrados = []
for C <- Cuentos do
  case C#cuento.cuento of
    Titulo -> CuentosFiltrados ++ [C]
    _ -> CuentosFiltrados
  end
end
```

La expresión `C#cuento.cuento` es equivalente a `C.cuento.cuento`.

El código utiliza la función `io:format/2` para imprimir los mensajes. La función `io:format/2` recibe una cadena de formato y una lista de argumentos, y imprime la cadena de formato con los argumentos insertados en los lugares apropiados.

La cadena de formato utilizada en este código es "~p". La cadena de formato "~p" se utiliza para imprimir un término Erlang en su representación textual.