```prolog
:- dynamic estudiante/3.
:- dynamic curso/3.
:- dynamic matricula/2.

estudiante(jose, ramirez, 18).
estudiante(maria, lopez, 19).
estudiante(pedro, garcia, 20).

curso(matematicas, algebra, 1).
curso(fisica, mecanica, 2).
curso(quimica, inorganica, 3).

matricula(jose, matematicas).
matricula(jose, fisica).
matricula(maria, quimica).
matricula(pedro, matematicas).

% Regla para obtener el nombre completo de un estudiante a partir de su número de matrícula
nombre_estudiante(Matricula, Nombre, Apellido) :-
    matricula(Matricula, Curso),
    curso(Curso, _, _),
    estudiante(Nombre, Apellido, _).

% Regla para obtener el nombre del curso a partir del número de matrícula de un estudiante
nombre_curso(Matricula, NombreCurso) :-
    matricula(Matricula, Curso),
    curso(Curso, NombreCurso, _).

% Regla para obtener el año de curso a partir del número de matrícula de un estudiante
anio_curso(Matricula, AnioCurso) :-
    matricula(Matricula, Curso),
    curso(Curso, _, AnioCurso).

% Regla para obtener la edad de un estudiante a partir de su número de matrícula
edad_estudiante(Matricula, Edad) :-
    matricula(Matricula, Curso),
    curso(Curso, _, _),
    estudiante(Nombre, Apellido, Edad).

% Regla para obtener el promedio de un estudiante a partir de su número de matrícula
promedio_estudiante(Matricula, Promedio) :-
    matricula(Matricula, Curso),
    curso(Curso, _, _),
    estudiante(Nombre, Apellido, _),
    findall(Nota, (calificacion(Nombre, Apellido, Curso, Nota)), Notas),
    sum_list(Notas, SumaNotas),
    length(Notas, CantidadNotas),
    Promedio is SumaNotas / CantidadNotas.

% Regla para obtener la lista de estudiantes matriculados en un curso
estudiantes_matriculados(Curso, Matriculas) :-
    findall(Matricula, (matricula(Matricula, Curso)), Matriculas).

% Regla para obtener la lista de cursos en los que está matriculado un estudiante
cursos_matriculados(Matricula, Cursos) :-
    findall(Curso, matricula(Matricula, Curso), Cursos).

% Regla para obtener la lista de estudiantes matriculados en un curso en un año específico
estudiantes_matriculados_anio(Curso, Anio, Matriculas) :-
    findall(Matricula, (matricula(Matricula, Curso), anio_curso(Matricula, Anio)), Matriculas).

% Regla para obtener la lista de cursos en los que está matriculado un estudiante en un año específico
cursos_matriculados_anio(Matricula, Anio, Cursos) :-
    findall(Curso, (matricula(Matricula, Curso), anio_curso(Matricula, Anio)), Cursos).

% Regla para obtener la lista de estudiantes que tienen un promedio mayor o igual a un valor específico
estudiantes_con_promedio_mayor_o_igual(Promedio, Matriculas) :-
    findall(Matricula, (promedio_estudiante(Matricula, PromedioAux), PromedioAux >= Promedio), Matriculas).

% Regla para obtener la lista de cursos en los que el promedio de los estudiantes es mayor o igual a un valor específico
cursos_con_promedio_mayor_o_igual(Promedio, Cursos) :-
    findall(Curso, (findall(Matricula, matricula(Matricula, Curso), Matriculas),
                   maplist(promedio_estudiante, Matriculas, Promedios),
                   sum_list(Promedios, SumaPromedios),
                   length(Promedios, CantidadPromedios),
                   PromedioCurso is SumaPromedios / CantidadPromedios,
                   PromedioCurso >= Promedio), Cursos).

% Regla para obtener la lista de estudiantes que tienen un promedio menor o igual a un valor específico
estudiantes_con_promedio_menor_o_igual(Promedio, Matriculas) :-
    findall(Matricula, (promedio_estudiante(Matricula, PromedioAux), PromedioAux =< Promedio), Matriculas).

% Regla para obtener la lista de cursos en los que el promedio de los estudiantes es menor o igual a un valor específico
cursos_con_promedio_menor_o_igual(Promedio, Cursos) :-
    findall(Curso, (findall(Matricula, matricula(Matricula, Curso), Matriculas),
                   maplist(promedio_estudiante, Matriculas, Promedios),
                   sum_list(Promedios, SumaPromedios),
                   length(Promedios, CantidadPromedios),
                   PromedioCurso is SumaPromedios / CantidadPromedios,
                   PromedioCurso =< Promedio), Cursos).
```

Este código en Prolog implementa una base de datos de estudiantes, cursos y matrículas. La base de datos se puede utilizar para almacenar información sobre los estudiantes, los cursos en los que están matriculados y sus calificaciones.

El código incluye una serie de reglas que permiten consultar la base de datos. Por ejemplo, se puede obtener el nombre completo de un estudiante a partir de su número de matrícula, el nombre del curso en el que está matriculado un estudiante, el año de curso en el que está matriculado un estudiante, la edad de un estudiante, el promedio de un estudiante, la lista de estudiantes matriculados en un curso, la lista de cursos en los que está matriculado un estudiante, la lista de estudiantes matriculados en un curso en un año específico, la lista de cursos en los que está matriculado un estudiante en un año específico, la lista de estudiantes que tienen un promedio mayor o igual a un valor específico, la lista de cursos en los que el promedio de los estudiantes es mayor o igual a un valor específico, la lista de estudiantes que tienen un promedio menor o igual a un valor específico y la lista de cursos en los que el promedio de los estudiantes es menor o igual a un valor específico.