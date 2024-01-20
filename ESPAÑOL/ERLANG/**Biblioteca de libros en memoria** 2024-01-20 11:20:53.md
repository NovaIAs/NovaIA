```erlang
-module(biblioteca).

-export([iniciar/0, agregar_libro/2, buscar_libro/1, eliminar_libro/1, listar_libros/0]).

-record(libro, {titulo, autor, editorial, anio}).

iniciar() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(libro, [{attributes, record_info(fields, libro)}]).

agregar_libro(Titulo, Autor) ->
    Libro = #libro{titulo=Titulo, autor=Autor},
    mnesia:write(Libro).

buscar_libro(Titulo) ->
    Libros = mnesia:read({libro, Titulo}),
    Libros.

eliminar_libro(Titulo) ->
    Libro = #libro{titulo=Titulo},
    mnesia:delete(Libro).

listar_libros() ->
    Libros = mnesia:all(libro),
    Libros.
```

Este código crea una base de datos en memoria usando Mnesia, una base de datos distribuida. Define una tabla de libros con los campos título, autor, editorial y año.

La función `iniciar()` crea la base de datos y la tabla en memoria, si ya existe la base de datos, la función `iniciar()` no hace nada.

La función `agregar_libro()` agrega un libro a la base de datos.

La función `buscar_libro()` busca un libro en la base de datos por su título y devuelve el libro si lo encuentra.

La función `eliminar_libro()` elimina un libro de la base de datos por su título.

La función `listar_libros()` devuelve todos los libros en la base de datos.