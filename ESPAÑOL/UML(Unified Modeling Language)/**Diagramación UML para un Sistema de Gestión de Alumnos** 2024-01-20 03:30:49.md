```
Diagrama de Clases:

- Sistema de Gestión de Alumnos:

  * Clase Alumno:
    - Atributos:
      - Nombre: String
      - Matrícula: String
      - Carrera: String
      - Promedio: Double
    - Métodos:
      - ObtenerNombre(): String
      - ObtenerMatrícula(): String
      - ObtenerCarrera(): String
      - ObtenerPromedio(): Double

  * Clase Profesor:
    - Atributos:
      - Nombre: String
      - Matrícula: String
      - Departamento: String
      - Área de Especialización: String
    - Métodos:
      - ObtenerNombre(): String
      - ObtenerMatrícula(): String
      - ObtenerDepartamento(): String
      - ObtenerÁreaDeEspecialización(): String

  * Clase Curso:
    - Atributos:
      - Nombre: String
      - Código: String
      - Créditos: Integer
      - Horario: String
    - Métodos:
      - ObtenerNombre(): String
      - ObtenerCódigo(): String
      - ObtenerCréditos(): Integer
      - ObtenerHorario(): String

  * Clase Inscripción:
    - Atributos:
      - Alumno: Alumno
      - Curso: Curso
      - Calificación: Double
    - Métodos:
      - ObtenerAlumno(): Alumno
      - ObtenerCurso(): Curso
      - ObtenerCalificación(): Double

- Diagrama de Secuencia:

  * Registro de un Alumno:
    - Alumno ingresa sus datos personales y académicos en el formulario de registro.
    - El sistema valida los datos ingresados y guarda al alumno en la base de datos.
    - El sistema genera un número de matrícula único para el alumno.

  * Inscripción de un Alumno en un Curso:
    - Alumno selecciona el curso que desea cursar en el catálogo de cursos.
    - El sistema verifica si el alumno cumple con los requisitos para cursar el curso.
    - El sistema registra la inscripción del alumno en el curso.

  * Carga de Calificaciones:
    - Profesor ingresa las calificaciones de los alumnos en el sistema.
    - El sistema guarda las calificaciones en la base de datos.

  * Consulta de Calificaciones:
    - Alumno puede consultar sus calificaciones en el sistema.
    - Profesor puede consultar las calificaciones de sus alumnos en el sistema.

- Diagrama de Actividades:

  * Proceso de Admisión de Alumnos:
    - El alumno solicita la admisión a la universidad.
    - La universidad evalúa la solicitud del alumno y decide si admitirlo o no.
    - Si el alumno es admitido, se le envía una carta de aceptación.

  * Proceso de Registro de Alumnos:
    - El alumno se registra en la universidad y paga las tasas de matrícula.
    - El alumno recibe un número de matrícula y un horario de clases.

  * Proceso de Inscripción de Alumnos en Cursos:
    - El alumno selecciona los cursos que desea cursar en el catálogo de cursos.
    - El sistema verifica si el alumno cumple con los requisitos para cursar los cursos.
    - El alumno se inscribe en los cursos seleccionados.

  * Proceso de Carga de Calificaciones:
    - El profesor ingresa las calificaciones de los alumnos en el sistema.
    - El sistema guarda las calificaciones en la base de datos.

  * Proceso de Consulta de Calificaciones:
    - El alumno puede consultar sus calificaciones en el sistema.
    - El profesor puede consultar las calificaciones de sus alumnos en el sistema.

- Diagrama de Casos de Uso:

  * Actor: Alumno
    - Casos de Uso:
      - Registrarse en la universidad
      - Inscribirse en cursos
      - Consultar calificaciones

  * Actor: Profesor
    - Casos de Uso:
      - Registrar calificaciones de alumnos
      - Consultar calificaciones de alumnos

  * Actor: Administrador
    - Casos de Uso:
      - Gestionar alumnos
      - Gestionar cursos
      - Gestionar inscripciones
      - Gestionar calificaciones

Explicación:

El código UML anterior es un modelo de un sistema de gestión de alumnos. El sistema consta de cuatro clases principales: Alumno, Profesor, Curso e Inscripción.

El diagrama de clases muestra las clases y sus relaciones. El diagrama de secuencia muestra el flujo de control de un caso de uso específico. El diagrama de actividades muestra el flujo de trabajo de un proceso específico. El diagrama de casos de uso muestra los actores del sistema y sus casos de uso.

El código UML anterior es un ejemplo de cómo utilizar UML para modelar un sistema complejo. UML es un lenguaje de modelado muy versátil que se puede utilizar para modelar una amplia variedad de sistemas.