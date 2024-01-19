**Diagrama de clases UML ampliado y diferenciado:**


**Clase Persona:**

- **Atributos:**
  - Nombre
  - Apellido
  - Edad
  - Ocupación


- **Métodos:**
  - ObtenerNombre()
  - ObtenerApellido()
  - ObtenerEdad()
  - ObtenerOcupación()
  - EstablecerNombre()
  - EstablecerApellido()
  - EstablecerEdad()
  - EstablecerOcupación()



**Clase Estudiante:**

- **Atributos:**
  - Matrícula
  - Especialidad
  - Semestre


- **Métodos:**
  - ObtenerMatrícula()
  - ObtenerEspecialidad()
  - ObtenerSemestre()
  - EstablecerMatrícula()
  - EstablecerEspecialidad()
  - EstablecerSemestre()


- **Relación con Persona:**
  - Herencia: Estudiante hereda de Persona


**Clase Profesor:**

- **Atributos:**
  - NúmDocente
  - Departamento
  - Rango


- **Métodos:**
  - ObtenerNúmDocente()
  - ObtenerDepartamento()
  - ObtenerRango()
  - EstablecerNúmDocente()
  - EstablecerDepartamento()
  - EstablecerRango()


- **Relación con Persona:**
  - Herencia: Profesor hereda de Persona


**Clase Asignatura:**

- **Atributos:**
  - Código
  - Nombre
  - Créditos


- **Métodos:**
  - ObtenerCódigo()
  - ObtenerNombre()
  - ObtenerCréditos()
  - EstablecerCódigo()
  - EstablecerNombre()
  - EstablecerCréditos()


**Clase Matrícula:**

- **Atributos:**
  - IdMatricula
  - Estudiante
  - Asignatura
  - Nota


- **Métodos:**
  - ObtenerIdMatricula()
  - ObtenerEstudiante()
  - ObtenerAsignatura()
  - ObtenerNota()
  - EstablecerIdMatricula()
  - EstablecerEstudiante()
  - EstablecerAsignatura()
  - EstablecerNota()


**Clase Departamento:**

- **Atributos:**
  - IdDepartamento
  - Nombre
  - Ubicación


- **Métodos:**
  - ObtenerIdDepartamento()
  - ObtenerNombre()
  - ObtenerUbicación()
  - EstablecerIdDepartamento()
  - EstablecerNombre()
  - EstablecerUbicación()


**Relaciones:**

- Asociación: Estudiante se asocia con Asignatura a través de Matrícula.
- Asociación: Profesor se asocia con Asignatura a través de Imparte.
- Asociación: Departamento se asocia con Profesor a través de Pertenece.


**Explicación del código:**

- El código UML ampliado y diferenciado modela un sistema de gestión académica. Incluye clases para representar personas, estudiantes, profesores, asignaturas, matrículas, departamentos e imparticiones.
- Las clases tienen atributos y métodos que representan sus características y acciones.
- Las relaciones entre las clases se muestran mediante asociaciones.
- Este código UML es más complejo y diferenciado que los códigos típicos, ya que incluye una amplia gama de clases y relaciones. Sin embargo, sigue siendo fácil de entender y usar para modelar un sistema de gestión académica.