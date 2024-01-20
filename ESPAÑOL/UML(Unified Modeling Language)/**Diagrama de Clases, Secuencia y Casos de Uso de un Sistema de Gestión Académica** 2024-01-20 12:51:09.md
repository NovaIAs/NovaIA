```
Diagrama de Clases

```
- Clase Persona:

 Atributos:
  - id: identificador único de la persona
  - nombre: nombre de la persona
  - apellidos: apellidos de la persona
  - fechaNacimiento: fecha de nacimiento de la persona
  - genero: género de la persona

 Métodos:
  - constructor: constructor de la clase Persona
  - getId: devuelve el identificador único de la persona
  - getNombre: devuelve el nombre de la persona
  - getApellidos: devuelve los apellidos de la persona
  - getFechaNacimiento: devuelve la fecha de nacimiento de la persona
  - getGenero: devuelve el género de la persona


- Clase Estudiante:

 Atributos:
  - id: identificador único del estudiante
  - nombre: nombre del estudiante
  - apellidos: apellidos del estudiante
  - fechaNacimiento: fecha de nacimiento del estudiante
  - genero: género del estudiante
  - matricula: matrícula del estudiante
  - carrera: carrera del estudiante

 Métodos:
  - constructor: constructor de la clase Estudiante
  - getId: devuelve el identificador único del estudiante
  - getNombre: devuelve el nombre del estudiante
  - getApellidos: devuelve los apellidos del estudiante
  - getFechaNacimiento: devuelve la fecha de nacimiento del estudiante
  - getGenero: devuelve el género del estudiante
  - getMatricula: devuelve la matrícula del estudiante
  - getCarrera: devuelve la carrera del estudiante


- Clase Profesor:

 Atributos:
  - id: identificador único del profesor
  - nombre: nombre del profesor
  - apellidos: apellidos del profesor
  - fechaNacimiento: fecha de nacimiento del profesor
  - genero: género del profesor
  - matricula: matrícula del profesor
  - departamento: departamento del profesor

 Métodos:
  - constructor: constructor de la clase Profesor
  - getId: devuelve el identificador único del profesor
  - getNombre: devuelve el nombre del profesor
  - getApellidos: devuelve los apellidos del profesor
  - getFechaNacimiento: devuelve la fecha de nacimiento del profesor
  - getGenero: devuelve el género del profesor
  - getMatricula: devuelve la matrícula del profesor
  - getDepartamento: devuelve el departamento del profesor



- Clase Curso:

 Atributos:
  - id: identificador único del curso
  - nombre: nombre del curso
  - descripcion: descripción del curso
  - creditos: créditos del curso
  - horario: horario del curso

 Métodos:
  - constructor: constructor de la clase Curso
  - getId: devuelve el identificador único del curso
  - getNombre: devuelve el nombre del curso
  - getDescripcion: devuelve la descripción del curso
  - getCreditos: devuelve los créditos del curso
  - getHorario: devuelve el horario del curso


- Clase Inscripcion:

 Atributos:
  - id: identificador único de la inscripción
  - estudiante: estudiante que se inscribe al curso
  - curso: curso al que se inscribe el estudiante
  - nota: nota obtenida por el estudiante en el curso

 Métodos:
  - constructor: constructor de la clase Inscripcion
  - getId: devuelve el identificador único de la inscripción
  - getEstudiante: devuelve el estudiante que se inscribe al curso
  - getCurso: devuelve el curso al que se inscribe el estudiante
  - getNota: devuelve la nota obtenida por el estudiante en el curso


```
Diagrama de Secuencia

```
1. El estudiante se registra en el sistema.
2. El sistema muestra al estudiante la lista de cursos disponibles.
3. El estudiante selecciona un curso.
4. El sistema muestra al estudiante la información del curso.
5. El estudiante se inscribe en el curso.
6. El sistema genera una inscripción para el estudiante.
7. El estudiante recibe un correo electrónico con la información de la inscripción.
8. El estudiante asiste al curso.
9. El profesor evalúa al estudiante.
10. El estudiante recibe una nota.
11. El estudiante consulta su nota en el sistema.
```


```
Diagrama de Casos de Uso

```
- Actor Estudiante:

 Casos de uso:
  - Registrarse en el sistema
  - Ver la lista de cursos disponibles
  - Seleccionar un curso
  - Inscribirse en un curso
  - Consultar la información de una inscripción
  - Asistir al curso
  - Consultar la nota de un curso


- Actor Profesor:

 Casos de uso:
  - Registrarse en el sistema
  - Crear un curso
  - Editar un curso
  - Eliminar un curso
  - Evaluar a los estudiantes
  - Publicar las notas de los estudiantes