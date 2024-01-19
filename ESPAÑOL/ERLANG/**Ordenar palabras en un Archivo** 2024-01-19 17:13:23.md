```erlang
-module(arreglar_archivo).
-export([leer_archivo/1, escribir_archivo/2, ordenar_archivo/1]).

leer_archivo(NombreArchivo) ->
    {ok, Contenidos} = file:read_file(NombreArchivo),
    Contenidos.

escribir_archivo(NombreArchivo, Contenidos) ->
    file:write_file(NombreArchivo, Contenidos).

ordenar_archivo(NombreArchivo) ->
    Contenidos = leer_archivo(NombreArchivo),
    Palabras = string:tokens(Contenidos, " ,.-!"),
    PalabrasOrdenadas = lists:sort(Palabras),
    escribir_archivo(NombreArchivo, string:join(PalabrasOrdenadas, " ")).
```

Este código en Erlang lee un archivo de texto, separa las palabras en un array, ordena el array y escribe el array ordenado en el archivo original.

El módulo se llama `arreglar_archivo` y tiene tres funciones: `leer_archivo/1`, `escribir_archivo/2` y `ordenar_archivo/1`.

La función `leer_archivo/1` toma un nombre de archivo como argumento y devuelve el contenido del archivo como una cadena.

La función `escribir_archivo/2` toma un nombre de archivo y una cadena como argumentos y escribe la cadena en el archivo.

La función `ordenar_archivo/1` toma un nombre de archivo como argumento, lee el contenido del archivo, separa las palabras en un array, ordena el array y escribe el array ordenado en el archivo original.

Para usar este código, puedes hacer lo siguiente:

```erlang
1> arreglar_archivo:ordenar_archivo("archivo.txt").
```

Esto ordenará las palabras en el archivo "archivo.txt" y escribirá el resultado en el mismo archivo.