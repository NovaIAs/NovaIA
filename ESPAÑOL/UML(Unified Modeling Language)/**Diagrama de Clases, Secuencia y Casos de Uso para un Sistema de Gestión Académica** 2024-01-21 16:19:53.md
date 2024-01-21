```
**Diagrama de Clases**

**Clase Persona**

* Atributos:
    * Nombre: String
    * Apellidos: String
    * Edad: Int
    * Dirección: String
    * Teléfono: String
    * Correo electrónico: String

* Métodos:
    * getNombre(): String
    * setNombre(String): Void
    * getApellidos(): String
    * setApellidos(String): Void
    * getEdad(): Int
    * setEdad(Int): Void
    * getDirección(): String
    * setDirección(String): Void
    * getTeléfono(): String
    * setTeléfono(String): Void
    * getCorreoElectrónico(): String
    * setCorreoElectrónico(String): Void

**Clase Estudiante**

* Atributos:
    * Número de matrícula: String
    * Carrera: String
    * Semestre: Int

* Métodos:
    * getNúmeroDeMatrícula(): String
    * setNúmeroDeMatrícula(String): Void
    * getCarrera(): String
    * setCarrera(String): Void
    * getSemestre(): Int
    * setSemestre(Int): Void

**Clase Profesor**

* Atributos:
    * Número de empleado: String
    * Departamento: String
    * Puesto: String

* Métodos:
    * getNúmeroDeEmpleado(): String
    * setNúmeroDeEmpleado(String): Void
    * getDepartamento(): String
    * setDepartamento(String): Void
    * getPuesto(): String
    * setPuesto(String): Void

**Clase Curso**

* Atributos:
    * Código: String
    * Nombre: String
    * Créditos: Int
    * Profesor: Profesor

* Métodos:
    * getCódigo(): String
    * setCódigo(String): Void
    * getNombre(): String
    * setNombre(String): Void
    * getCréditos(): Int
    * setCréditos(Int): Void
    * getProfesor(): Profesor
    * setProfesor(Profesor): Void

**Clase Horario**

* Atributos:
    * Día: String
    * Hora: String
    * Aula: String
    * Curso: Curso

* Métodos:
    * getDía(): String
    * setDía(String): Void
    * getHora(): String
    * setHora(String): Void
    * getAula(): String
    * setAula(String): Void
    * getCurso(): Curso
    * setCurso(Curso): Void

**Diagrama de Secuencia**

1. El estudiante se registra en el sistema.
2. El estudiante selecciona los cursos que desea tomar.
3. El sistema verifica que el estudiante tenga los requisitos previos para los cursos seleccionados.
4. Si el estudiante cumple con los requisitos previos, el sistema registra al estudiante en los cursos seleccionados.
5. El estudiante recibe una notificación por correo electrónico con el horario de sus clases.
6. El estudiante asiste a las clases y participa en las actividades académicas.
7. Al final del semestre, el estudiante recibe sus calificaciones.
8. El estudiante puede consultar sus calificaciones en el sistema.

**Diagrama de Casos de Uso**

* Actor: Estudiante
* Caso de uso: Registrarse en el sistema
* Descripción: El estudiante se registra en el sistema para poder acceder a los servicios en línea.
* Precondiciones: El estudiante debe tener una cuenta de correo electrónico válida.
* Postcondiciones: El estudiante queda registrado en el sistema y recibe un correo electrónico de confirmación.

* Actor: Estudiante
* Caso de uso: Seleccionar cursos
* Descripción: El estudiante selecciona los cursos que desea tomar en el próximo semestre.
* Precondiciones: El estudiante debe estar registrado en el sistema.
* Postcondiciones: El estudiante queda inscrito en los cursos seleccionados.

* Actor: Estudiante
* Caso de uso: Consultar calificaciones
* Descripción: El estudiante consulta sus calificaciones en el sistema.
* Precondiciones: El estudiante debe estar registrado en el sistema.
* Postcondiciones: El estudiante puede ver sus calificaciones en el sistema.

**Explicación del Código**

El código anterior es un ejemplo de un diagrama de clases, un diagrama de secuencia y un diagrama de casos de uso para un sistema de gestión académica. El diagrama de clases describe las clases principales del sistema y sus relaciones. El diagrama de secuencia describe el flujo de eventos en el sistema. El diagrama de casos de uso describe los casos de uso del sistema y sus actores.