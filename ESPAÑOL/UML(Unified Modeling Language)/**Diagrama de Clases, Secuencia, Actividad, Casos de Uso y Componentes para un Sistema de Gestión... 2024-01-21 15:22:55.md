```
**Diagrama de Clases**

**Clases:**

* **Persona:**
    * Atributos:
        * Nombre
        * Apellido
        * Edad
        * Género
    * Métodos:
        * GetNombre()
        * GetApellido()
        * GetEdad()
        * GetGénero()
* **Estudiante:**
    * Atributos:
        * Matrícula
        * Carrera
        * Facultad
    * Métodos:
        * GetMatrícula()
        * GetCarrera()
        * GetFacultad()
* **Profesor:**
    * Atributos:
        * Número de empleado
        * Departamento
        * Salario
    * Métodos:
        * GetNúmeroDeEmpleado()
        * GetDepartamento()
        * GetSalario()
* **Curso:**
    * Atributos:
        * Código
        * Nombre
        * Créditos
    * Métodos:
        * GetCódigo()
        * GetNombre()
        * GetCréditos()
* **Inscripción:**
    * Atributos:
        * Estudiante
        * Curso
        * Calificación
    * Métodos:
        * GetEstudiante()
        * GetCurso()
        * GetCalificación()

**Relaciones:**

* **Persona** es padre de **Estudiante** y **Profesor**.
* **Estudiante** tiene una relación de muchos a muchos con **Curso** a través de la clase **Inscripción**.
* **Profesor** tiene una relación de muchos a muchos con **Curso** a través de la clase **Inscripción**.

**Diagrama de Secuencia**

**Mensaje 1:** El estudiante se inscribe en un curso.
**Mensaje 2:** El sistema verifica que el estudiante tenga los requisitos previos para el curso.
**Mensaje 3:** Si el estudiante tiene los requisitos previos, el sistema lo inscribe en el curso.
**Mensaje 4:** Si el estudiante no tiene los requisitos previos, el sistema le notifica que no puede inscribirse en el curso.

**Diagrama de Actividad**

**Actividad 1:** El estudiante selecciona los cursos en los que quiere inscribirse.
**Actividad 2:** El sistema verifica que el estudiante tenga los requisitos previos para los cursos.
**Actividad 3:** Si el estudiante tiene los requisitos previos, el sistema lo inscribe en los cursos.
**Actividad 4:** Si el estudiante no tiene los requisitos previos, el sistema le notifica que no puede inscribirse en los cursos.

**Diagrama de Casos de Uso**

**Actores:**

* Estudiante
* Profesor
* Administrador

**Casos de Uso:**

* **Estudiante:**
    * Inscribirse en un curso
    * Ver sus calificaciones
    * Retirarse de un curso
* **Profesor:**
    * Crear un curso
    * Agregar estudiantes a un curso
    * Calificar a los estudiantes
* **Administrador:**
    * Crear usuarios
    * Asignar permisos a los usuarios
    * Ver informes

**Diagrama de Componentes**

**Componentes:**

* **Interfaz de usuario:**
    * Interfaz gráfica de usuario
    * Controles de entrada de datos
    * Controles de salida de datos
* **Motor de base de datos:**
    * Base de datos relacional
    * Gest