```
:- dynamic profesor/2.
:- dynamic alumno/2.
:- dynamic asignatura/2.
:- dynamic matricula/4.

% Reglas para el profesor

profesor(nombre, apellido).

% Reglas para el alumno

alumno(nombre, apellido).

% Reglas para la asignatura

asignatura(nombre, departamento).

% Reglas para la matrícula

matricula(alumno, asignatura, curso, nota).

% Consulta

% Buscar todos los alumnos que están matriculados en una asignatura

?- findall(Alumno, matricula(Alumno, Asignatura, _, _), Alumnos),
   write('Los alumnos matriculados en ', Asignatura, ' son: '),
   write(Alumnos),
   nl.

% Buscar todos los profesores que imparten una asignatura

?- findall(Profesor, profesor(Nombre, Apellido), Profesores),
   write('Los profesores que imparten una asignatura son: '),
   write(Profesores),
   nl.

% Buscar todas las asignaturas que se imparten en un departamento

?- findall(Asignatura, asignatura(Asignatura, Departamento), Asignaturas),
   write('Las asignaturas que se imparten en ', Departamento, ' son: '),
   write(Asignaturas),
   nl.

% Buscar la nota de un alumno en una asignatura

?- matricula(Alumno, Asignatura, _, Nota),
   write('La nota de ', Alumno, ' en ', Asignatura, ' es: '),
   write(Nota),
   nl.

% Añadir un nuevo profesor

?- assert(profesor(Juan, Pérez)).

% Añadir un nuevo alumno

?- assert(alumno(Ana, López)).

% Añadir una nueva asignatura

?- assert(asignatura(Matemáticas, Ciencias)).

% Matricular a un alumno en una asignatura

?- assert(matricula(Ana, Matemáticas, 1, 9)).

% Eliminar un profesor

?- retract(profesor(Juan, Pérez)).

% Eliminar un alumno

?- retract(alumno(Ana, López)).

% Eliminar una asignatura

?- retract(asignatura(Matemáticas, Ciencias)).

% Desmatricular a un alumno de una asignatura

?- retract(matricula(Ana, Matemáticas, 1, 9)).
```

Este código es un sistema de gestión de matrículas en PROLOG. Contiene reglas para definir profesores, alumnos, asignaturas y matrículas. También contiene reglas para consultar el sistema, como buscar todos los alumnos que están matriculados en una asignatura, buscar todos los profesores que imparten una asignatura, buscar todas las asignaturas que se imparten en un departamento y buscar la nota de un alumno en una asignatura. El sistema también permite añadir nuevos profesores, alumnos, asignaturas y matrículas, y eliminar profesores, alumnos, asignaturas y matrículas.