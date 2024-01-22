```
**Diagrama de Casos de Uso:**

* **Actor:** Paciente
* **Caso de Uso:** Gestionar Citas Médicas
    * **Flujo Principal:**
        1. El paciente inicia sesión en el sistema.
        2. El paciente selecciona la opción "Gestionar Citas Médicas".
        3. El paciente ingresa la fecha y hora de la cita deseada.
        4. El sistema verifica la disponibilidad del médico.
        5. Si el médico está disponible, el sistema programa la cita.
        6. Si el médico no está disponible, el sistema ofrece al paciente otras opciones de fecha y hora.
        7. El paciente selecciona una de las opciones ofrecidas o cancela la cita.
        8. El sistema envía una confirmación de la cita al paciente por correo electrónico o mensaje de texto.
    * **Flujo Alternativo:**
        1. El paciente no tiene una cuenta en el sistema.
        2. El paciente crea una cuenta en el sistema.
        3. El paciente inicia sesión en el sistema.
        4. El paciente continúa con el flujo principal.

**Diagrama de Clases:**

* **Clase:** Paciente
    * **Atributos:**
        * Nombre
        * Apellido
        * Fecha de nacimiento
        * Número de teléfono
        * Correo electrónico
        * Dirección
        * Historial médico
    * **Métodos:**
        * Iniciar sesión
        * Cerrar sesión
        * Gestionar citas médicas
        * Ver historial médico
        * Actualizar información personal
* **Clase:** Médico
    * **Atributos:**
        * Nombre
        * Apellido
        * Especialidad
        * Horario de atención
        * Número de teléfono
        * Correo electrónico
        * Dirección
    * **Métodos:**
        * Iniciar sesión
        * Cerrar sesión
        * Atender pacientes
        * Programar citas
        * Ver historial médico de los pacientes
        * Actualizar información personal
* **Clase:** Cita Médica
    * **Atributos:**
        * Fecha
        * Hora
        * Médico
        * Paciente
        * Motivo de la cita
    * **Métodos:**
        * Programar cita
        * Cancelar cita
        * Ver información de la cita

**Diagrama de Secuencia:**

* **Objeto:** Paciente
* **Evento:** Gestionar Citas Médicas
    * **Mensaje:** Iniciar sesión
    * **Respuesta:** Iniciar sesión
    * **Mensaje:** Seleccionar opción "Gestionar Citas Médicas"
    * **Respuesta:** Mostrar opciones de gestión de citas médicas
    * **Mensaje:** Ingresar fecha y hora de la cita deseada
    * **Respuesta:** Verificar disponibilidad del médico
    * **Mensaje:** Programar cita
    * **Respuesta:** Enviar confirmación de la cita al paciente

**Diagrama de Colaboración:**

* **Objetos:** Paciente, Médico, Cita Médica
* **Relaciones:**
    * El paciente programa una cita con el médico.
    * El médico atiende al paciente en la cita programada.
    * El médico registra el historial médico del paciente en la cita.

**Diagrama de Estado:**

* **Objeto:** Cita Médica
* **Estados:**
    * Programada
    * Cancelada
    * Atendida
* **Transiciones:**
    * Programada -> Cancelada
    * Programada -> Atendida
```