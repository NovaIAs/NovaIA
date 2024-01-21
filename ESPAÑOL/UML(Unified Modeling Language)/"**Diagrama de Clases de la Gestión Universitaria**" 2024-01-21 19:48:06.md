```
Sistemas de Información para la Gestión Universitaria

Diagrama de Clases

-------------------------------------------------------------------------------------------
|                                                                                                         |
|                                         Entidad                          Elemento               |
|                                                                                                         |
-------------------------------------------------------------------------------------------
|                            |                    |                                |
|                             |                    |                                |
|            Estudiante      |                    |                                |
|                             |                    |                                |
|     - idEstudiante     |                    |                                |
|     - nombreEstudiante  |                    |                                |
|     - apellidoEstudiante |                    |                                |
|     - carreraEstudiante |                    |                                |
|                             |                    |                                |
|                            |                    |                                |
-------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------
|                                                                                                         |
|                                          Entidad                       Elemento               |
|                                                                                                         |
-------------------------------------------------------------------------------------------
|                            |                        |                                |
|                             |                        |                                |
|              Profesor     |                        |                                |
|                             |                        |                                |
|      - idProfesor      |                        |                                |
|      - nombreProfesor   |                        |                                |
|      - apellidoProfesor |                        |                                |
|      - especialidadProfesor |                        |                                |
|                             |                        |                                |
|                            |                        |                                |
-------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------
|                                                                                                         |
|                                          Entidad                       Elemento               |
|                                                                                                         |
-------------------------------------------------------------------------------------------
|                            |                    |                                |
|                             |                    |                                |
|             Curso        |                    |                                |
|                             |                    |                                |
|      - idCurso         |                    |                                |
|      - nombreCurso      |                    |                                |
|      - creditosCurso    |                    |                                |
|      - semestreCurso   |                    |                                |
|                             |                    |                                |
|                            |                    |                                |
-------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------
|                                                                                                         |
|                                         Entidad                         Elemento               |
|                                                                                                         |
-------------------------------------------------------------------------------------------
|                            |                   |                                |
|                             |                   |                                |
|   Inscripción Curso    |                   |                                |
|                             |                   |                                |
|  - idInscripcionCurso  |                   |                                |
|  - idEstudianteIC    |                   |                                |
|  - idCursoIC          |                   |                                |
|  - notaEstudianteIC  |                   |                                |
|                             |                   |                                |
|                            |                   |                                |
-------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------
|                                                                                                         |
|                                          Entidad                        Elemento               |
|                                                                                                         |
-------------------------------------------------------------------------------------------
|                            |                     |                                |
|                             |                     |                                |
|          Calificaciones   |                     |                                |
|                             |                     |                                |
|   - idCalificaciones  |                     |                                |
|   - idEstudianteC    |                     |                                |
|   - idCursoC          |                     |                                |
|   - notaEstudianteC  |                     |                                |
|                             |                     |                                |
|                            |                     |                                |
-------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------
|                                                                                                         |
|                                       Asociación                        Cardinalidad Elementos |
|                                                                                                         |
-------------------------------------------------------------------------------------------
|                            |                   |                                |
|                             |                   |                                |
| Estudiante - Curso      |                   | Estudiante (1) - Curso (N)  |
|            Cursada       |                   |                             |
|                             |                   |                                |
|                            |                   |                                |
-------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------
|                                                                                                         |
|                                       Asociación                         Cardinalidad Elementos |
|                                                                                                         |
-------------------------------------------------------------------------------------------
|                            |                   |                                |
|                             |                   |                                |
| Estudiante - Calificaciones |                   | Estudiante (1) - Calificaciones (N) |
|                 Toma      |                   |                             |
|                             |                   |                                |
|                            |                   |                                |
-------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------
|                                                                                                         |
|                                       Asociación                         Cardinalidad Elementos |
|                                                                                                         |
-------------------------------------------------------------------------------------------
|                            |                   |                                |
|                             |                   |                                |
| Profesor - Curso         |                   | Profesor (1) - Curso (N)     |
|            Dicta          |                   |                             |
|                             |                   |                                |
|                            |                   |                                |
-------------------------------------------------------------------------------------------

**Explicación del código:**

* El diagrama de clases muestra las entidades y las relaciones entre ellas.
* Las entidades son los objetos principales del sistema. En este caso, las entidades son Estudiante, Profesor, Curso, Inscripción Curso y Calificaciones.
* Las relaciones son las asociaciones entre las entidades. En este caso, las relaciones son Cursada, Toma y Dicta.
* La cardinalidad de una relación es el número de instancias de una entidad que pueden estar relacionadas con una sola instancia de otra entidad. En este caso, la cardinalidad de la relación Cursada es Estudiante (1) - Curso (N), lo que significa que un estudiante puede cursar muchos cursos, pero un curso solo puede ser cursado por un estudiante.
* Los elementos de una relación son las propiedades que describen la relación. En este caso, los elementos de la relación Cursada son idInscripcionCurso, idEstudianteIC e idCursoIC.