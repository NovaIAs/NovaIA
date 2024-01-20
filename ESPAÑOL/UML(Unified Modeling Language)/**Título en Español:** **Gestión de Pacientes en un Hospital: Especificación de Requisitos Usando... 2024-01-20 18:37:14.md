**Caso de Uso: Gestión de Pacientes en un Hospital**

**Diagrama de Casos de Uso**

**Actor:** Paciente

**Caso de Uso:** Solicitar Cita Médica

**Precondición:** El paciente está registrado en el hospital.

**Flujo de Eventos:**

1. El paciente llama al hospital y solicita una cita médica.
2. El operador del hospital registra la solicitud de cita y la envía al departamento correspondiente.
3. El departamento correspondiente asigna un médico al paciente y le envía una notificación de la cita.
4. El paciente acude a la cita médica y es atendido por el médico.
5. El médico realiza el diagnóstico y prescribe un tratamiento.
6. El paciente recibe el tratamiento y se recupera.

**Postcondición:** El paciente ha recibido el tratamiento adecuado y se ha recuperado.

**Diagrama de Clases**

**Clase:** Paciente

**Atributos:**

* Nombre
* Apellidos
* Edad
* Sexo
* Dirección
* Teléfono
* Correo electrónico

**Métodos:**

* Solicitar Cita Médica
* Acudir a Cita Médica
* Recibir Tratamiento

**Clase:** Médico

**Atributos:**

* Nombre
* Apellidos
* Especialidad
* Número de Colegiado
* Dirección
* Teléfono
* Correo electrónico

**Métodos:**

* Atender Paciente
* Diagnosticar Paciente
* Prescribir Tratamiento

**Clase:** Cita Médica

**Atributos:**

* Fecha
* Hora
* Lugar
* Médico
* Paciente

**Métodos:**

* Crear Cita Médica
* Modificar Cita Médica
* Cancelar Cita Médica

**Clase:** Tratamiento

**Atributos:**

* Nombre
* Descripción
* Dosis
* Frecuencia
* Duración

**Métodos:**

* Aplicar Tratamiento
* Modificar Tratamiento
* Suspender Tratamiento

**Diagrama de Secuencia**

**Diagrama de Comunicación**

**Explicación:**

El código UML anterior describe un sistema de gestión de pacientes en un hospital. El sistema permite a los pacientes solicitar citas médicas, acudir a las citas y recibir tratamiento. También permite a los médicos atender pacientes, diagnosticar enfermedades y prescribir tratamientos.

El diagrama de casos de uso muestra los diferentes actores del sistema y los casos de uso que pueden realizar. El diagrama de clases muestra las diferentes clases del sistema y sus atributos y métodos. El diagrama de secuencia muestra la secuencia de eventos que se produce cuando un paciente solicita una cita médica. El diagrama de comunicación muestra los diferentes objetos del sistema que interactúan entre sí para procesar una solicitud de cita médica.