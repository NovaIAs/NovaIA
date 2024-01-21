**Sistema de Gestión de Alumnos**

**Caso de Uso:** Registro de Alumno

**Diagrama de Clases:**

```
+----------------+
| Alumno        |
+----------------+
| - id_alumno   |
| - nombre       |
| - apellido     |
| - fecha_nacimiento |
| - sexo         |
| - telefono     |
| - email        |
+----------------+

+-------------------+
| Curso            |
+-------------------+
| - id_curso       |
| - nombre          |
| - descripcion     |
| - fecha_inicio    |
| - fecha_fin      |
| - horario        |
| - lugar           |
+-------------------+

+---------------------------+
| Alumno_Curso            |
+---------------------------+
| - id_alumno_curso       |
| - id_alumno            |
| - id_curso             |
| - nota                 |
| - asistencia           |
+---------------------------+

+----------------+
| Profesor       |
+----------------+
| - id_profesor  |
| - nombre        |
| - apellido      |
| - fecha_nacimiento |
| - sexo          |
| - telefono      |
| - email         |
+----------------+

+----------------------+
| Curso_Profesor       |
+----------------------+
| - id_curso_profesor   |
| - id_curso            |
| - id_profesor         |
+----------------------+

+----------------+
| Calificacion   |
+----------------+
| - id_calificacion   |
| - id_alumno_curso   |
| - nota              |
| - fecha             |
+----------------+

**Diagrama de Secuencia:**

```
1. El alumno envía una petición de registro al sistema.
2. El sistema valida los datos del alumno y crea una nueva instancia de la clase Alumno.
3. El sistema envía un correo electrónico de confirmación al alumno.
4. El alumno hace clic en el enlace de confirmación del correo electrónico.
5. El sistema activa la cuenta del alumno.
6. El alumno inicia sesión en el sistema.
7. El alumno selecciona un curso de la lista de cursos disponibles.
8. El sistema añade el alumno al curso.
9. El alumno puede ver el contenido del curso, enviar tareas y realizar exámenes.
10. El profesor puede ver las tareas y los exámenes de los alumnos, y calificarlos.
11. El sistema calcula la nota final del alumno en el curso.
12. El alumno puede ver su nota final en el sistema.
```