```
Diagrama de Clases UML

Clase Persona:
- Atributos:
  - Nombre: String
  - Apellido: String
  - Edad: Integer
  - Género: String
- Métodos:
  - GetNombre(): String
  - GetApellido(): String
  - GetEdad(): Integer
  - GetGénero(): String

Clase Estudiante:
- Hereda de Persona
- Atributos:
  - Carrera: String
  - Matrícula: String
  - Calificaciones: List<Calificación>
- Métodos:
  - GetCarrera(): String
  - GetMatrícula(): String
  - GetCalificaciones(): List<Calificación>

Clase Profesor:
- Hereda de Persona
- Atributos:
  - Área de Especialización: String
  - Años de Experiencia: Integer
- Métodos:
  - GetÁrea de Especialización(): String
  - GetAños de Experiencia(): Integer

Clase Curso:
- Atributos:
  - Nombre: String
  - Código: String
  - Créditos: Integer
- Métodos:
  - GetNombre(): String
  - GetCódigo(): String
  - GetCréditos(): Integer

Clase Calificación:
- Atributos:
  - Curso: Curso
  - Estudiante: Estudiante
  - Nota: Float
- Métodos:
  - GetCurso(): Curso
  - GetEstudiante(): Estudiante
  - GetNota(): Float

Diagrama de Secuencia UML

Caso de Uso: Registro de un Nuevo Estudiante

Inicio:
- El usuario introduce los datos del nuevo estudiante en el formulario de registro.

Paso 1:
- El sistema valida los datos introducidos por el usuario.

Paso 2:
- Si los datos son válidos, el sistema crea un nuevo objeto Estudiante y lo añade a la lista de estudiantes.

Paso 3:
- El sistema muestra un mensaje de confirmación al usuario.

Paso 4:
- Fin.

Diagrama de Actividades UML

Caso de Uso: Inscripción de un Estudiante en un Curso

Inicio:
- El estudiante selecciona el curso en el que desea inscribirse.

Paso 1:
- El sistema verifica si el estudiante cumple con los requisitos previos para el curso.

Paso 2:
- Si el estudiante cumple con los requisitos previos, el sistema añade al estudiante a la lista de estudiantes inscritos en el curso.

Paso 3:
- El sistema notifica al estudiante de su inscripción en el curso.

Paso 4:
- Fin.

Explicación del Código:

El código anterior es un ejemplo de cómo utilizar UML para modelar un sistema. El diagrama de clases define las clases y sus relaciones, el diagrama de secuencia define un caso de uso y el diagrama de actividades define otro caso de uso.

Los diagramas de UML son una herramienta valiosa para el desarrollo de software, ya que permiten visualizar y comprender el sistema de una manera clara y concisa.
```