```
Diagrama de Clases

- Clase Persona:
  - Atributos:
    - Nombre: string
    - Apellido: string
    - Edad: integer
  - Métodos:
    - getNombre(): string
    - setNombre(nombre: string): void
    - getApellido(): string
    - setApellido(apellido: string): void
    - getEdad(): integer
    - setEdad(edad: integer): void

- Clase Profesor:
  - Hereda de la clase Persona
  - Atributos:
    - Materia: string
    - Años de experiencia: integer
  - Métodos:
    - getMateria(): string
    - setMateria(materia: string): void
    - getAñosDeExperiencia(): integer
    - setAñosDeExperiencia(añosDeExperiencia: integer): void

- Clase Estudiante:
  - Hereda de la clase Persona
  - Atributos:
    - Grado: string
    - Promedio: float
  - Métodos:
    - getGrado(): string
    - setGrado(grado: string): void
    - getPromedio(): float
    - setPromedio(promedio: float): void

- Clase Curso:
  - Atributos:
    - Nombre: string
    - Profesor: Profesor
    - Estudiantes: List<Estudiante>
  - Métodos:
    - getNombre(): string
    - setNombre(nombre: string): void
    - getProfesor(): Profesor
    - setProfesor(profesor: Profesor): void
    - getEstudiantes(): List<Estudiante>
    - setEstudiantes(estudiantes: List<Estudiante>): void

Diagrama de Secuencia

- Escenario: Un estudiante se inscribe en un curso
  - Actor: Estudiante
  - Flujo de eventos:
    1. El estudiante selecciona un curso.
    2. El sistema verifica si el estudiante cumple con los requisitos del curso.
    3. Si el estudiante cumple con los requisitos, el sistema lo inscribe en el curso.
    4. Si el estudiante no cumple con los requisitos, el sistema le notifica que no puede inscribirse en el curso.

Diagrama de Casos de Uso

- Caso de uso: Inscripción de estudiantes
  - Actores:
    - Estudiante
    - Sistema
  - Precondiciones:
    - El estudiante debe tener una cuenta válida en el sistema.
    - El estudiante debe haber seleccionado un curso.
  - Flujo de eventos:
    1. El estudiante inicia sesión en el sistema.
    2. El estudiante selecciona el curso en el que desea inscribirse.
    3. El sistema verifica si el estudiante cumple con los requisitos del curso.
    4. Si el estudiante cumple con los requisitos, el sistema lo inscribe en el curso.
    5. Si el estudiante no cumple con los requisitos, el sistema le notifica que no puede inscribirse en el curso.
  - Postcondiciones:
    - El estudiante está inscrito en el curso.
    - El sistema envía una notificación al profesor del curso informándole que un nuevo estudiante se ha inscrito.

Espero que este código complejo en UML te sea útil. El código está en español y es muy amplio y diferenciado, por lo que es difícil que se repita nuevamente.