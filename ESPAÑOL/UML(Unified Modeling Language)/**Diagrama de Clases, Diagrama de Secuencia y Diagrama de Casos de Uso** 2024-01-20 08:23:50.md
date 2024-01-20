**Diagrama de Clases**

| Clase | Atributos | Métodos |
|---|---|---|
| **Persona** | Nombre, Apellido, Edad | Hablar(), Caminar(), Comer() |
| **Estudiante** | Matrícula, Promedio | Estudiar(), TomarExámenes(), AsistirAClases() |
| **Profesor** | Número de Empleado, Sueldo | Enseñar(), CalificarExámenes(), AsistirAReuniones() |
| **Universidad** | Nombre, Dirección, Número de Estudiantes | OfrecerCursos(), AdmitirEstudiantes(), ExpedirDiplomas() |
| **Curso** | Nombre, Código, Créditos | Impartirse(), InscribirEstudiantes(), CalificarEstudiantes() |

**Diagrama de Secuencia**

1. El estudiante se inscribe en un curso.
2. El profesor imparte el curso.
3. El estudiante asiste a clases y toma exámenes.
4. El profesor califica los exámenes.
5. El estudiante recibe una nota.

**Diagrama de Casos de Uso**

| Caso de Uso | Actor | Precondición | Postcondición | Flujo de Eventos |
|---|---|---|---|---|
| **Inscribirse en un curso** | Estudiante | El estudiante está admitido en la universidad. | El estudiante está inscrito en el curso. |
| **Impartir un curso** | Profesor | El profesor está asignado al curso. | El curso se ha impartido. |
| **Asistir a clases y tomar exámenes** | Estudiante | El estudiante está inscrito en el curso. | El estudiante ha asistido a clases y ha tomado exámenes. |
| **Calificar los exámenes** | Profesor | El profesor está asignado al curso. | Los exámenes han sido calificados. |
| **Recibir una nota** | Estudiante | El estudiante ha tomado exámenes. | El estudiante ha recibido una nota. |