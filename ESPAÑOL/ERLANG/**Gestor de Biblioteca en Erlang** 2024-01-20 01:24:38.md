```erlang
% Definición de un módulo en Erlang
-module(biblioteca).

% Definición de una función en Erlang
-export([añadir_libro/2, eliminar_libro/2, buscar_libro/1]).

% Estructura para representar un libro
-record(libro, {titulo, autor, año}).

% Base de datos de libros
-spec libros() -> [libro()].
libros() -> [].

% Función para añadir un libro a la base de datos
-spec añadir_libro(libro(), libros()) -> libros().
añadir_libro(Libro, Libros) -> [Libro | Libros].

% Función para eliminar un libro de la base de datos
-spec eliminar_libro(libro(), libros()) -> libros().
eliminar_libro(Libro, Libros) -> lists:delete(Libro, Libros).

% Función para buscar un libro en la base de datos
-spec buscar_libro(titulo()) -> libro().
buscar_libro(Título) -> lists:filter(fun(Libro) -> Libro#libro.titulo == Título end, libros()).

% Ejemplo de uso del módulo
-main(_) ->
    Libros = añadir_libro(#libro{titulo = "Cien años de soledad", autor = "Gabriel García Márquez", año = 1967}, libros()),
    Libros = añadir_libro(#libro{titulo = "El Quijote", autor = "Miguel de Cervantes Saavedra", año = 1605}, Libros),
    Libros = añadir_libro(#libro{titulo = "La Divina Comedia", autor = "Dante Alighieri", año = 1308}, Libros),
    Libro = buscar_libro("Cien años de soledad"),
    io:format("Libro encontrado: ~p~n", [Libro]),
    io:format("Libros en la base de datos: ~p~n", [Libros]).
```

**Explicación del código:**

* El módulo `biblioteca` define tres funciones para gestionar una base de datos de libros:

    * `añadir_libro/2`: Añade un libro a la base de datos.
    * `eliminar_libro/2`: Elimina un libro de la base de datos.
    * `buscar_libro/1`: Busca un libro en la base de datos por su título.

* La estructura `libro` se utiliza para representar un libro.

* La función `libros/0` devuelve la base de datos de libros, que inicialmente está vacía.

* La función `añadir_libro/2` añade un libro a la base de datos.

* La función `eliminar_libro/2` elimina un libro de la base de datos.

* La función `buscar_libro/1` busca un libro en la base de datos por su título.

* La función `-main/1` se utiliza para probar el módulo.

* La función `io:format/2` se utiliza para imprimir texto en la consola.