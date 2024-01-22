```prolog
% Definición de hechos y reglas para representar un sistema de gestión de bibliotecas.

% Hechos que representan libros en la biblioteca.

libro(1, 'El Quijote', 'Miguel de Cervantes', 1605).
libro(2, 'Hamlet', 'William Shakespeare', 1603).
libro(3, 'La Divina Comedia', 'Dante Alighieri', 1320).

% Hechos que representan préstamos de libros.

prestamo(1, 1, 'Juan Pérez', '2023-03-08').
prestamo(2, 2, 'Ana García', '2023-02-15').
prestamo(3, 3, 'Pedro Sánchez', '2023-01-22').

% Regla para encontrar el título de un libro a partir de su ID.

titulo_libro(ID, Titulo) :-
    libro(ID, Titulo, _, _).

% Regla para encontrar el autor de un libro a partir de su ID.

autor_libro(ID, Autor) :-
    libro(ID, _, Autor, _).

% Regla para encontrar el año de publicación de un libro a partir de su ID.

publicacion_libro(ID, Anio) :-
    libro(ID, _, _, Anio).

% Regla para encontrar la fecha de préstamo de un libro a partir de su ID de préstamo.

fecha_prestamo(IDPrestamo, Fecha) :-
    prestamo(IDPrestamo, _, _, Fecha).

% Regla para encontrar el nombre del prestatario de un libro a partir de su ID de préstamo.

prestatario_libro(IDPrestamo, Nombre) :-
    prestamo(IDPrestamo, _, Nombre, _).

% Regla para encontrar todos los libros prestados por un determinado prestatario.

libros_prestados_por(Prestatario, Libros) :-
    findall(ID, (prestamo(_, ID, Prestatario, _), libro(ID, _, _, _)), Libros).

% Regla para encontrar todos los prestatarios que han prestado un determinado libro.

prestatarios_que_han_prestado(Libro, Prestatarios) :-
    findall(Nombre, (prestamo(_, Libro, Nombre, _)), Prestatarios).

% Regla para encontrar todos los préstamos que se han realizado en un determinado año.

prestamos_en_anio(Anio, Prestamos) :-
    findall((IDPrestamo, IDLibro, Nombre, Fecha), (prestamo(IDPrestamo, IDLibro, Nombre, Fecha), year(Fecha, Anio)), Prestamos).

% Regla para encontrar todos los libros que se han prestado en un determinado año.

libros_prestados_en_anio(Anio, Libros) :-
    findall(IDLibro, (prestamo(_, IDLibro, _, Fecha), year(Fecha, Anio)), Libros).

% Regla para encontrar todos los prestatarios que han prestado libros en un determinado año.

prestatarios_que_han_prestado_en_anio(Anio, Prestatarios) :-
    findall(Nombre, (prestamo(_, _, Nombre, Fecha), year(Fecha, Anio)), Prestatarios).

```

Este código representa un sistema de gestión de bibliotecas en Prolog. Incluye hechos que representan libros en la biblioteca y préstamos de libros, así como reglas para encontrar información sobre libros, prestatarios y préstamos.