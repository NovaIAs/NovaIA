**Diagrama de Clases**

```
+----------------+
| Clase Persona |
+----------------+
| - nombre: String |
| - edad: int |
| - sexo: String |
| ----------------- |
| + constructor(nombre, edad, sexo) |
| + getNombre(): String |
| + getEdad(): int |
| + getSexo(): String |
+----------------+

+-----------------+
| Clase Estudiante |
+-----------------+
| - notaMedia: double |
| ----------------- |
| + constructor(nombre, edad, sexo, notaMedia) |
| + getNotaMedia(): double |
+-----------------+

+---------------------+
| Clase Trabajador |
+---------------------+
| - salario: double |
| ----------------- |
| + constructor(nombre, edad, sexo, salario) |
| + getSalario(): double |
+---------------------+

+---------------------+
| Clase Empleado |
+---------------------+
| - departamento: String |
| ----------------- |
| + constructor(nombre, edad, sexo, departamento) |
| + getDepartamento(): String |
+---------------------+

+-----------------------+
| Clase Profesor |
+-----------------------+
| - asignatura: String |
| ----------------- |
| + constructor(nombre, edad, sexo, asignatura) |
| + getAsignatura(): String |
+-----------------------+
```

**Diagrama de Secuencia**

```
Estudiante -> Universidad: Solicitar Admisión

Universidad -> Estudiante: Aceptar Admisión

Estudiante -> Universidad: Matricularse

Universidad -> Estudiante: Emitir Tarjeta de Estudiante

Estudiante -> Profesor: Solicitar Cita

Profesor -> Estudiante: Asignar Cita

Estudiante -> Profesor: Acudir a Cita

Profesor -> Estudiante: Evaluar Trabajo

Estudiante -> Profesor: Recibir Evaluación

Estudiante -> Universidad: Solicitar Graduación

Universidad -> Estudiante: Emitir Diploma
```

**Diagrama de Casos de Uso**

```
Actor: Estudiante

Casos de Uso:

1. Solicitar Admisión
2. Matricularse
3. Solicitar Cita
4. Acudir a Cita
5. Solicitar Graduación

Actor: Profesor

Casos de Uso:

1. Asignar Cita
2. Evaluar Trabajo
```