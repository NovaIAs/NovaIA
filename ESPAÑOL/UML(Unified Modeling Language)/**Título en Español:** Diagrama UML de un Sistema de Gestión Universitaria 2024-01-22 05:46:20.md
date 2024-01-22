```
Diagrama de Clases

Clase Persona {
- Nombre: String
- Edad: Integer
- Dirección: String
- Teléfono: String
- Correo electrónico: String
}

Clase Estudiante : Persona {
- Matrícula: String
- Carrera: String
- Semestre: Integer
}

Clase Profesor : Persona {
- Título: String
- Departamento: String
- Años de experiencia: Integer
}

Clase Curso {
- Código: String
- Nombre: String
- Créditos: Integer
- Horario: String
}

Clase Inscripción {
- Estudiante: Estudiante
- Curso: Curso
- Calificación: Integer
}

Clase Universidad {
- Nombre: String
- Dirección: String
- Teléfono: String
- Correo electrónico: String
}

Diagrama de Casos de Uso

Actor Estudiante {
- Registrarse
- Iniciar sesión
- Ver cursos
- Inscribirse en cursos
- Ver calificaciones
- Pagar matrícula
}

Actor Profesor {
- Registrarse
- Iniciar sesión
- Crear cursos
- Ver estudiantes inscritos
- Ingresar calificaciones
}

Actor Universidad {
- Gestionar estudiantes
- Gestionar profesores
- Gestionar cursos
- Gestionar inscripciones
- Generar reportes
}

Diagrama de Secuencia

Caso de Uso: Registrarse (Estudiante)

1. El estudiante accede al sistema de la universidad.
2. El estudiante ingresa sus datos personales (nombre, edad, dirección, teléfono, correo electrónico).
3. El sistema valida los datos ingresados.
4. El sistema crea una cuenta para el estudiante.
5. El estudiante recibe un correo electrónico de confirmación.

Caso de Uso: Inscribirse en cursos (Estudiante)

1. El estudiante accede al sistema de la universidad.
2. El estudiante selecciona los cursos en los que desea inscribirse.
3. El sistema verifica si el estudiante cumple con los requisitos para inscribirse en los cursos seleccionados.
4. El estudiante paga la matrícula.
5. El sistema inscribe al estudiante en los cursos seleccionados.

Caso de Uso: Ingresar calificaciones (Profesor)

1. El profesor accede al sistema de la universidad.
2. El profesor selecciona el curso en el que desea ingresar calificaciones.
3. El sistema muestra la lista de estudiantes inscritos en el curso.
4. El profesor ingresa las calificaciones de los estudiantes.
5. El sistema guarda las calificaciones ingresadas.

Diagrama de Actividades

Diagrama de Actividades: Sistema de gestión universitaria

1. El estudiante accede al sistema de la universidad.
2. El estudiante ingresa sus datos personales.
3. El sistema valida los datos ingresados.
4. El sistema crea una cuenta para el estudiante.
5. El estudiante recibe un correo electrónico de confirmación.
6. El estudiante selecciona los cursos en los que desea inscribirse.
7. El sistema verifica si el estudiante cumple con los requisitos para inscribirse en los cursos seleccionados.
8. El estudiante paga la matrícula.
9. El sistema inscribe al estudiante en los cursos seleccionados.
10. El profesor accede al sistema de la universidad.
11. El profesor selecciona el curso en el que desea ingresar calificaciones.
12. El sistema muestra la lista de estudiantes inscritos en el curso.
13. El profesor ingresa las calificaciones de los estudiantes.
14. El sistema guarda las calificaciones ingresadas.
15. El estudiante accede al sistema de la universidad.
16. El estudiante ve sus calificaciones.
17. El estudiante paga la matrícula.

Explicación del código

El código anterior es un modelo UML de un sistema de gestión universitaria. El modelo incluye un diagrama de clases, un diagrama de casos de uso, un diagrama de secuencia y un diagrama de actividades.

El diagrama de clases define las clases del sistema, sus atributos y sus relaciones. Las clases principales del sistema son Persona, Estudiante, Profesor, Curso, Inscripción y Universidad.

El diagrama de casos de uso define los casos de uso del sistema y los actores que participan en cada caso de uso. Los actores principales del sistema son Estudiante, Profesor y Universidad.

El diagrama de secuencia muestra el flujo de eventos en el caso de uso de registro de un estudiante. El diagrama muestra cómo el estudiante accede al sistema, ingresa sus datos personales, el sistema valida los datos ingresados, el sistema crea una cuenta para el estudiante y el estudiante recibe un correo electrónico de confirmación.

El diagrama de actividades muestra el flujo de actividades en el sistema de gestión universitaria. El diagrama muestra cómo el estudiante accede al sistema, se registra, se inscribe en cursos, ve sus calificaciones y paga la matrícula. El diagrama también muestra cómo el profesor accede al sistema, crea cursos, ingresa calificaciones y guarda las calificaciones ingresadas.