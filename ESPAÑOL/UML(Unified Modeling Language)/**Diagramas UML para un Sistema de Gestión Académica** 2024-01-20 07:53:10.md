**Diagrama de Clases**

```
+--------------------------------+
| Clase: Persona                 |
+--------------------------------+
| Atributos:                    |
| - Nombre: string              |
| - Apellido: string            |
| - Edad: int                   |
| Métodos:                     |
| - Constructor(nombre, apellido, edad) |
| - GetNombre(): string          |
| - GetApellido(): string        |
| - GetEdad(): int              |
+--------------------------------+

+--------------------------------+
| Clase: Estudiante              |
+--------------------------------+
| Atributos:                    |
| - Matrícula: string           |
| - Carrera: string             |
| - Promedio: float             |
| Métodos:                     |
| - Constructor(matrícula, carrera, promedio) |
| - GetMatrícula(): string       |
| - GetCarrera(): string         |
| - GetPromedio(): float         |
+--------------------------------+

+--------------------------------+
| Clase: Profesor                |
+--------------------------------+
| Atributos:                    |
| - Número de empleado: string   |
| - Departamento: string         |
| - Sueldo: float               |
| Métodos:                     |
| - Constructor(número de empleado, departamento, sueldo) |
| - GetNúmeroDeEmpleado(): string  |
| - GetDepartamento(): string    |
| - GetSueldo(): float           |
+--------------------------------+

+--------------------------------+
| Clase: Curso                  |
+--------------------------------+
| Atributos:                    |
| - Código: string              |
| - Nombre: string              |
| - Créditos: int               |
| Métodos:                     |
| - Constructor(código, nombre, créditos) |
| - GetCódigo(): string          |
| - GetNombre(): string          |
| - GetCréditos(): int           |
+--------------------------------+

+--------------------------------+
| Clase: Inscripción              |
+--------------------------------+
| Atributos:                    |
| - Estudiante: Estudiante      |
| - Curso: Curso                |
| - Calificación: float          |
| Métodos:                     |
| - Constructor(estudiante, curso, calificación) |
| - GetEstudiante(): Estudiante  |
| - GetCurso(): Curso            |
| - GetCalificación(): float     |
+--------------------------------+
```

**Diagrama de Objetos**

```
+--------------------------------+
| Instancia: Persona1            |
+--------------------------------+
| Nombre: Juan                   |
| Apellido: Pérez                |
| Edad: 20                       |
+--------------------------------+

+--------------------------------+
| Instancia: Persona2            |
+--------------------------------+
| Nombre: María                  |
| Apellido: González              |
| Edad: 22                       |
+--------------------------------+

+--------------------------------+
| Instancia: Estudiante1          |
+--------------------------------+
| Matrícula: 123456789         |
| Carrera: Ingeniería Informática |
| Promedio: 3.8                  |
+--------------------------------+

+--------------------------------+
| Instancia: Estudiante2          |
+--------------------------------+
| Matrícula: 987654321         |
| Carrera: Medicina              |
| Promedio: 4.0                  |
+--------------------------------+

+--------------------------------+
| Instancia: Profesor1            |
+--------------------------------+
| Número de empleado: 12345     |
| Departamento: Informática      |
| Sueldo: 50000                 |
+--------------------------------+

+--------------------------------+
| Instancia: Profesor2            |
+--------------------------------+
| Número de empleado: 67890     |
| Departamento: Medicina         |
| Sueldo: 60000                 |
+--------------------------------+

+--------------------------------+
| Instancia: Curso1               |
+--------------------------------+
| Código: INF101                |
| Nombre: Introducción a la Informática |
| Créditos: 3                    |
+--------------------------------+

+--------------------------------+
| Instancia: Curso2               |
+--------------------------------+
| Código: MED101                |
| Nombre: Introducción a la Medicina |
| Créditos: 4                    |
+--------------------------------+

+--------------------------------+
| Instancia: Inscripción1         |
+--------------------------------+
| Estudiante: Estudiante1        |
| Curso: Curso1                  |
| Calificación: 3.5              |
+--------------------------------+

+--------------------------------+
| Instancia: Inscripción2         |
+--------------------------------+
| Estudiante: Estudiante2        |
| Curso: Curso2                  |
| Calificación: 4.0              |
+--------------------------------+
```

**Diagrama de Casos de Uso**

```
+--------------------------------+
| Caso de uso: Inscribirse a un curso |
+--------------------------------+
| Actores:                       |
| - Estudiante                   |
| - Profesor                     |
| Flujo principal:               |
| 1. El estudiante selecciona un curso. |
| 2. El estudiante envía la solicitud de inscripción al profesor. |
| 3. El profesor aprueba o rechaza la solicitud. |
| 4. Si la solicitud es aprobada, el estudiante se inscribe en el curso. |
| 5. Si la solicitud es rechazada, el estudiante recibe una notificación. |
+--------------------------------+

+--------------------------------+
| Caso de uso: Consultar calificaciones |
+--------------------------------+
| Actores:                       |
| - Estudiante                   |
| - Profesor                     |
| Flujo principal:               |
| 1. El estudiante selecciona un curso. |
| 2. El estudiante envía la solicitud de consulta de calificaciones al profesor. |
| 3. El profesor envía las calificaciones del estudiante al estudiante. |
+--------------------------------+
```

**Diagrama de Secuencia**

```
+--------------------------------+
| Diagrama de secuencia: Inscribirse a un curso |
+--------------------------------+
| Objetos:                       |
| - Estudiante                   |
| - Profesor                     |
| Mensajes:                      |
| 1. Estudiante -> Profesor: Solicitud de inscripción |
| 2. Profesor -> Estudiante: Aprobación o rechazo de la solicitud |
| 3. Estudiante -> Profesor: Inscripción en el curso |
+--------------------------------+

+--------------------------------+
| Diagrama de secuencia: Consultar calificaciones |
+--------------------------------+
| Objetos:                       |
| - Estudiante                   |
| - Profesor                     |
| Mensajes:                      |
| 1. Estudiante -> Profesor: Solicitud de consulta de calificaciones |
| 2. Profesor -> Estudiante: Calificaciones del estudiante |
+--------------------------------+
```

**Explicación del Código**

El diagrama de clases define las clases del sistema, sus atributos y métodos. Las clases principales son Persona, Estudiante, Profesor, Curso e Inscripción.

El diagrama de objetos muestra las instancias de las clases del sistema. Las instancias principales son Persona1, Persona2, Estudiante1, Estudiante2, Profesor1, Profesor2, Curso1, Curso2, Inscripción1 e Inscripción2.

El diagrama de casos de uso define los casos de uso del sistema. Los casos de uso principales son Inscribirse a un curso y Consultar calificaciones.

El diagrama de secuencia muestra la secuencia de mensajes entre los objetos del sistema en los casos de uso. Los diagramas de secuencia principales son Inscribirse a un curso y Consultar calificaciones.

El código anterior es un ejemplo de un sistema de gestión académica. El sistema permite a los estudiantes inscribirse en cursos, consultar sus calificaciones y a los profesores aprobar o rechazar solicitudes de inscripción y enviar las calificaciones de los estudiantes.