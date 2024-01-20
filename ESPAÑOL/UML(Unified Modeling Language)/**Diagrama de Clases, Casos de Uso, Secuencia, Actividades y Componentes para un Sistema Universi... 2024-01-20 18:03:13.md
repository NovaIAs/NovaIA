**Diagrama de Clases**

```
Clase Persona {
    - Nombre: String
    - Apellido: String
    - Edad: Integer
    - Direccion: String
    - Telefono: String
}

Clase Estudiante : Persona {
    - Matricula: String
    - Carrera: String
    - Semestre: Integer
}

Clase Profesor : Persona {
    - CedulaProfesional: String
    - Especialidad: String
    - AñosDeExperiencia: Integer
}

Clase Curso {
    - Nombre: String
    - Creditos: Integer
    - Horario: String
    - Salon: String
}

Clase Inscripcion {
    - Estudiante: Estudiante
    - Curso: Curso
    - Calificacion: Float
}

Clase Universidad {
    - Nombre: String
    - Direccion: String
    - Telefono: String
    - ListaDeEstudiantes: List<Estudiante>
    - ListaDeProfesores: List<Profesor>
    - ListaDeCursos: List<Curso>
    - ListaDeInscripciones: List<Inscripcion>
}
```

**Diagrama de Casos de Uso**

```
Actor Estudiante:

    - Registrarse
    - Inscribirse en cursos
    - Ver calificaciones
    - Solicitar beca

Actor Profesor:

    - Registrarse
    - Crear cursos
    - Actualizar calificaciones
    - Consultar lista de estudiantes

Actor Administrador:

    - Crear universidad
    - Agregar estudiantes
    - Agregar profesores
    - Agregar cursos
    - Generar reportes
```

**Diagrama de Secuencia**

```
Estudiante -> Universidad: Registrarse
Universidad -> Estudiante: Confirmar registro
Estudiante -> Universidad: Inscribirse en cursos
Universidad -> Estudiante: Confirmar inscripción
Estudiante -> Universidad: Ver calificaciones
Universidad -> Estudiante: Mostrar calificaciones
```

**Diagrama de Actividades**

```
Estudiante:

    - Registrarse
    - Inscribirse en cursos
    - Ver calificaciones
    - Solicitar beca

Profesor:

    - Registrarse
    - Crear cursos
    - Actualizar calificaciones
    - Consultar lista de estudiantes

Administrador:

    - Crear universidad
    - Agregar estudiantes
    - Agregar profesores
    - Agregar cursos
    - Generar reportes
```

**Diagrama de Componentes**

```
Sistema Universitario:

    - Módulo de Registro
    - Módulo de Inscripción
    - Módulo de Calificaciones
    - Módulo de Becas

Módulo de Registro:

    - Formulario de registro de estudiantes
    - Formulario de registro de profesores
    - Formulario de registro de cursos

Módulo de Inscripción:

    - Formulario de inscripción de estudiantes en cursos
    - Listado de cursos disponibles
    - Listado de estudiantes inscritos

Módulo de Calificaciones:

    - Formulario de actualización de calificaciones
    - Listado de calificaciones de los estudiantes
    - Listado de cursos con sus calificaciones promedio

Módulo de Becas:

    - Formulario de solicitud de beca
    - Listado de becas disponibles
    - Listado de estudiantes beneficiarios de becas
```

**Explicación del Código**

El código UML anterior describe un sistema universitario completo. El diagrama de clases define las principales clases del sistema, como Persona, Estudiante, Profesor, Curso, Inscripción y Universidad. El diagrama de casos de uso define los actores del sistema (Estudiante, Profesor y Administrador) y los casos de uso que pueden realizar. El diagrama de secuencia describe el flujo de mensajes entre los actores y el sistema para realizar un caso de uso específico. El diagrama de actividades describe las actividades que se realizan para completar un caso de uso. El diagrama de componentes describe los componentes del sistema y sus relaciones.

El código UML anterior es un ejemplo complejo de cómo se puede utilizar UML para diseñar un sistema software. UML es un lenguaje de modelado visual que se utiliza para especificar, visualizar y documentar los sistemas de software. UML es una herramienta valiosa para los desarrolladores de software, ya que les ayuda a entender y comunicar los requisitos del sistema, a diseñar la arquitectura del sistema y a implementar el sistema.