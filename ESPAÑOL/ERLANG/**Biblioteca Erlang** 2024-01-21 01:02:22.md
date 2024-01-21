```erlang
-module(biblioteca).
-export([agregar_libro/3, buscar_libro/1, eliminar_libro/1, listar_libros/0]).

-record(libro, {titulo, autor, anio}).

-define(BIBLIOTECA, ets:new(biblioteca, [set])).

agregar_libro(Titulo, Autor, Anio) ->
    Libro = #libro{titulo=Titulo, autor=Autor, anio=Anio},
    ets:insert(?BIBLIOTECA, Libro).

buscar_libro(Titulo) ->
    ets:lookup(?BIBLIOTECA, Titulo).

eliminar_libro(Titulo) ->
    ets:delete(?BIBLIOTECA, Titulo).

listar_libros() ->
    ets:select(?BIBLIOTECA, [{#libro, '_', ['$_']}]).
```

Este código implementa una biblioteca simple en Erlang. La biblioteca tiene las siguientes funciones:

* `agregar_libro/3`: Agrega un libro a la biblioteca.
* `buscar_libro/1`: Busca un libro en la biblioteca.
* `eliminar_libro/1`: Elimina un libro de la biblioteca.
* `listar_libros/0`: Lista todos los libros de la biblioteca.

La biblioteca se implementa utilizando una tabla ets. Una tabla ets es una estructura de datos similar a una base de datos que se puede utilizar para almacenar y recuperar datos.

El código define un registro llamado `libro` que se utiliza para representar un libro en la biblioteca. El registro tiene tres campos: `titulo`, `autor` y `anio`.

El código también define una macro llamada `?BIBLIOTECA` que se utiliza para referirse a la tabla ets que almacena los libros.

La función `agregar_libro/3` agrega un libro a la biblioteca. La función toma como argumentos el título del libro, el autor del libro y el año de publicación del libro. La función crea un nuevo registro de libro y lo inserta en la tabla ets.

La función `buscar_libro/1` busca un libro en la biblioteca. La función toma como argumento el título del libro que se quiere buscar. La función busca el libro en la tabla ets y devuelve el registro del libro si se encuentra.

La función `eliminar_libro/1` elimina un libro de la biblioteca. La función toma como argumento el título del libro que se quiere eliminar. La función elimina el libro de la tabla ets.

La función `listar_libros/0` lista todos los libros de la biblioteca. La función selecciona todos los registros de libros de la tabla ets y los devuelve en una lista.