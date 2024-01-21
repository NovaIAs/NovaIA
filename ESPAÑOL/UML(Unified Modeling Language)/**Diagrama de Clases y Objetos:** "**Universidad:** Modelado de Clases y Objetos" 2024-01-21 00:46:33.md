**Diagrama de Clases:**

```
+-----------------------------+
| Clase: Persona              |
+-----------------------------+
| Atributos:                 |
| - nombre: String            |
| - edad: int                |
| - sexo: String              |
+-----------------------------+
| Métodos:                  |
| - hablar(): void           |
| - caminar(): void          |
| - comer(): void            |
+-----------------------------+

+--------------------------------+
| Clase: Estudiante             |
+--------------------------------+
| Atributos:                   |
| - matricula: String           |
| - carrera: String             |
| - semestre: int             |
+--------------------------------+
| Métodos:                    |
| - estudiar(): void           |
| - tomarExamenes(): void     |
| - hacerTareas(): void        |
+--------------------------------+

+---------------------------------+
| Clase: Profesor               |
+---------------------------------+
| Atributos:                   |
| - titulo: String              |
| - materia: String             |
| - salario: float            |
+---------------------------------+
| Métodos:                    |
| - darClase(): void           |
| - calificarExamenes(): void |
| - investigar(): void        |
+---------------------------------+

+--------------------------------------+
| Clase: Universidad                  |
+--------------------------------------+
| Atributos:                         |
| - nombre: String                   |
| - direccion: String                 |
| - telefono: String                 |
+--------------------------------------+
| Métodos:                          |
| - matricularEstudiante(): void      |
| - contratarProfesor(): void        |
| - impartirClase(): void           |
+--------------------------------------+
```

**Diagrama de Objetos:**

```
+--------------------------------------+
| Universidad: Universidad Nacional    |
+--------------------------------------+
| - nombre: Universidad Nacional    |
| - direccion: Ciudad de México      |
| - telefono: 555-555-5555       |
+--------------------------------------+

+--------------------------------------+
| Persona: Juan Pérez                 |
+--------------------------------------+
| - nombre: Juan Pérez               |
| - edad: 20                         |
| - sexo: Masculino                 |
+--------------------------------------+

+--------------------------------------+
| Estudiante: María López            |
+--------------------------------------+
| - nombre: María López              |
| - edad: 22                         |
| - sexo: Femenino                  |
| - matricula: 1234567890         |
| - carrera: Ingeniería en Sistemas  |
| - semestre: 8                     |
+--------------------------------------+

+--------------------------------------+
| Profesor: Carlos Ramírez            |
+--------------------------------------+
| - nombre: Carlos Ramírez          |
| - edad: 40                         |
| - sexo: Masculino                 |
| - titulo: Ingeniero en Sistemas   |
| - materia: Programación Avanzada |
| - salario: 20000                  |
+--------------------------------------+
```

**Diagrama de Secuencia:**

```
+--------------------------------------------------+
| Estudiante: Juan Pérez se matricula en la Universidad |
+--------------------------------------------------+
| 1. Juan Pérez llena la solicitud de matrícula.     |
| 2. La Universidad recibe la solicitud de matrícula. |
| 3. La Universidad verifica la información de la solicitud.|
| 4. La Universidad emite un número de matrícula a Juan Pérez. |
| 5. Juan Pérez paga la matrícula.                   |
| 6. La Universidad registra a Juan Pérez como estudiante. |
+--------------------------------------------------+

+--------------------------------------------------------+
| Profesor: Carlos Ramírez imparte una clase de Programación Avanzada |
+--------------------------------------------------------+
| 1. Carlos Ramírez llega al salón de clases.             |
| 2. Carlos Ramírez saluda a los estudiantes.           |
| 3. Carlos Ramírez explica el tema de la clase.          |
| 4. Los estudiantes toman notas y hacen preguntas.    |
| 5. Carlos Ramírez resuelve las dudas de los estudiantes. |
| 6. Carlos Ramírez evalúa el aprendizaje de los estudiantes. |
+--------------------------------------------------------+

+------------------------------------------------+
| Estudiante: María López toma un examen de Programación Avanzada |
+------------------------------------------------+
| 1. María López llega al salón de clases.        |
| 2. María López saluda al profesor.             |
| 3. El profesor entrega el examen a María López. |
| 4. María López resuelve el examen.             |
| 5. María López entrega el examen al profesor.   |
| 6. El profesor evalúa el examen de María López. |
+------------------------------------------------+
```

**Explicación del Código:**

El código UML anterior representa un sistema universitario simple. El sistema consta de las siguientes clases:

* **Persona:** Esta clase representa a las personas que interactúan con el sistema, ya sean estudiantes, profesores o personal administrativo.
* **Estudiante:** Esta clase representa a los estudiantes que se matriculan en la universidad.
* **Profesor:** Esta clase representa a los profesores que imparten clases en la universidad.
* **Universidad:** Esta clase representa a la universidad en sí.

El código UML también incluye tres diagramas:

* **Diagrama de Clases:** Este diagrama muestra las clases del sistema y sus relaciones.
* **Diagrama de Objetos:** Este diagrama muestra los objetos del sistema y sus relaciones.
* **Diagrama de Secuencia:** Este diagrama muestra la secuencia de eventos que ocurren en el sistema cuando se realiza una determinada tarea.

El código UML anterior es un ejemplo de cómo se puede utilizar UML para modelar un sistema. UML es un lenguaje de modelado que se utiliza para representar sistemas de software. UML puede utilizarse para crear diagramas de clases, diagramas de objetos, diagramas de secuencia y otros tipos de diagramas. UML es una herramienta valiosa para los desarrolladores de software, ya que les permite crear modelos visuales de sus sistemas que pueden ayudarles a comprender y diseñar mejor sus sistemas.