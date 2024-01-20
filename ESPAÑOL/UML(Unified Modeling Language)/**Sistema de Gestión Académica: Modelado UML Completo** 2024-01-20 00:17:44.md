```
Diagrama de Clases:

Clase Persona:
- Atributos:
  - Nombre: String
  - Edad: Integer
  - Direccion: String
- Métodos:
  - getNombre(): String
  - setNombre(nombre: String): void
  - getEdad(): Integer
  - setEdad(edad: Integer): void
  - getDireccion(): String
  - setDireccion(direccion: String): void

Clase Estudiante:
- Atributos:
  - Nombre: String
  - Edad: Integer
  - Direccion: String
  - Matricula: String
  - Carrera: String
- Métodos:
  - getNombre(): String
  - setNombre(nombre: String): void
  - getEdad(): Integer
  - setEdad(edad: Integer): void
  - getDireccion(): String
  - setDireccion(direccion: String): void
  - getMatricula(): String
  - setMatricula(matricula: String): void
  - getCarrera(): String
  - setCarrera(carrera: String): void

Clase Profesor:
- Atributos:
  - Nombre: String
  - Edad: Integer
  - Direccion: String
  - Cedula: String
  - Departamento: String
- Métodos:
  - getNombre(): String
  - setNombre(nombre: String): void
  - getEdad(): Integer
  - setEdad(edad: Integer): void
  - getDireccion(): String
  - setDireccion(direccion: String): void
  - getCedula(): String
  - setCedula(cedula: String): void
  - getDepartamento(): String
  - setDepartamento(departamento: String): void

Clase Curso:
- Atributos:
  - Nombre: String
  - Codigo: String
  - Creditos: Integer
  - Profesor: Profesor
- Métodos:
  - getNombre(): String
  - setNombre(nombre: String): void
  - getCodigo(): String
  - setCodigo(codigo: String): void
  - getCreditos(): Integer
  - setCreditos(creditos: Integer): void
  - getProfesor(): Profesor
  - setProfesor(profesor: Profesor): void

Diagrama de Secuencia:

1. El estudiante se registra en el sistema.
2. El sistema valida las credenciales del estudiante.
3. El estudiante selecciona los cursos que desea tomar.
4. El sistema verifica la disponibilidad de los cursos.
5. El estudiante se matricula en los cursos.
6. El sistema genera un horario de clases para el estudiante.
7. El estudiante asiste a las clases.
8. El profesor evalúa el desempeño del estudiante.
9. El sistema calcula las notas del estudiante.
10. El estudiante recibe sus calificaciones.

Diagrama de Actividades:

1. El estudiante inicia el proceso de inscripción.
2. El sistema verifica las credenciales del estudiante.
3. El estudiante selecciona los cursos que desea tomar.
4. El sistema verifica la disponibilidad de los cursos.
5. El estudiante se matricula en los cursos.
6. El sistema genera un horario de clases para el estudiante.
7. El estudiante asiste a las clases.
8. El profesor evalúa el desempeño del estudiante.
9. El sistema calcula las notas del estudiante.
10. El estudiante recibe sus calificaciones.

Diagrama de Casos de Uso:

- Actor: Estudiante
- Casos de Uso:
  - Registrarse
  - Iniciar sesión
  - Seleccionar cursos
  - Matricularse en cursos
  - Ver horario de clases
  - Asistir a clases
  - Realizar evaluaciones
  - Ver calificaciones

- Actor: Profesor
- Casos de Uso:
  - Registrarse
  - Iniciar sesión
  - Crear cursos
  - Modificar cursos
  - Eliminar cursos
  - Evaluar estudiantes
  - Calificar estudiantes

- Actor: Administrador
- Casos de Uso:
  - Registrarse
  - Iniciar sesión
  - Crear usuarios
  - Modificar usuarios
  - Eliminar usuarios
  - Gestionar cursos
  - Gestionar horarios
  - Gestionar calificaciones
```

Explicación:

El diagrama de clases define las clases del sistema y sus relaciones. Las clases Persona, Estudiante, Profesor y Curso representan los objetos principales del sistema.

El diagrama de secuencia muestra la secuencia de pasos que se siguen en el proceso de inscripción de un estudiante.

El diagrama de actividades muestra las actividades que se realizan en el proceso de inscripción de un estudiante.

El diagrama de casos de uso identifica los actores del sistema y los casos de uso que pueden realizar.

Este código pretende ser un ejemplo complejo y diferenciado de UML en español, cubriendo diferentes aspectos de un sistema de gestión académica. El código incluye diagramas de clases, de secuencia, de actividades y de casos de uso, todos ellos relacionados con el mismo sistema.