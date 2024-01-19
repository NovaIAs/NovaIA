**Diagrama de Clases**

```
+----------------+
| Persona       |
+----------------+
| - id          |
| - nombre      |
| - apellido    |
| - edad        |
| - direccion   |
| - telefono    |
| - correo      |
+----------------+

+----------------------+
| Mascota              |
+----------------------+
| - id                  |
| - nombre              |
| - especie             |
| - raza                |
| - edad                |
| - dueño                |
+----------------------+

+-----------------+
| Cita              |
+-----------------+
| - id                |
| - fecha              |
| - hora               |
| - motivo             |
| - mascota            |
| - veterinario        |
+-----------------+

+----------------------+
| Veterinario          |
+----------------------+
| - id                  |
| - nombre              |
| - apellido            |
| - especialidad        |
| - direccion           |
| - telefono            |
| - correo              |
+----------------------+

+-----------------+
| Tratamiento      |
+-----------------+
| - id                |
| - nombre              |
| - descripcion         |
| - dosis               |
| - frecuencia          |
| - duración            |
| - mascota            |
| - veterinario        |
+-----------------+

+-----------------+
| Receta            |
+-----------------+
| - id                |
| - nombre              |
| - descripcion         |
| - dosis               |
| - frecuencia          |
| - duración            |
| - mascota            |
| - veterinario        |
+-----------------+

+-----------------+
| Pago              |
+-----------------+
| - id                |
| - fecha              |
| - hora               |
| - monto              |
| - forma              |
| - cliente            |
| - veterinario        |
+-----------------+
```

**Diagrama de Secuencia**

```
Cliente -> Veterinario: Solicitar cita
Veterinario -> Cliente: Aceptar cita

Cliente -> Mascota: Llevar a la mascota a la cita
Mascota -> Veterinario: Presentar síntomas

Veterinario -> Mascota: Realizar examen
Veterinario -> Mascota: Diagnosticar enfermedad

Veterinario -> Cliente: Informar diagnóstico
Cliente -> Veterinario: Aceptar tratamiento

Veterinario -> Farmacia: Solicitar medicamentos
Farmacia -> Veterinario: Entregar medicamentos

Veterinario -> Cliente: Entregar medicamentos
Cliente -> Mascota: Administrar medicamentos

Mascota -> Veterinario: Recuperarse de la enfermedad
Veterinario -> Cliente: Informar recuperación

Cliente -> Veterinario: Pagar consulta
Veterinario -> Cliente: Aceptar pago
```

**Diagrama de Actividades**

```
Cliente solicita cita
Veterinario acepta cita
Cliente lleva a la mascota a la cita
Mascota presenta síntomas
Veterinario realiza examen
Veterinario diagnostica enfermedad
Veterinario informa diagnóstico
Cliente acepta tratamiento
Veterinario solicita medicamentos
Farmacia entrega medicamentos
Veterinario entrega medicamentos
Cliente administra medicamentos
Mascota se recupera de la enfermedad
Veterinario informa recuperación
Cliente paga consulta
Veterinario acepta pago
```

**Diagrama de Casos de Uso**

```
Actor: Cliente
Caso de uso: Solicitar cita

Actor: Veterinario
Caso de uso: Aceptar cita

Actor: Cliente
Caso de uso: Llevar a la mascota a la cita

Actor: Mascota
Caso de uso: Presentar síntomas

Actor: Veterinario
Caso de uso: Realizar examen

Actor: Veterinario
Caso de uso: Diagnosticar enfermedad

Actor: Veterinario
Caso de uso: Informar diagnóstico

Actor: Cliente
Caso de uso: Aceptar tratamiento

Actor: Veterinario
Caso de uso: Solicitar medicamentos

Actor: Farmacia
Caso de uso: Entregar medicamentos

Actor: Veterinario
Caso de uso: Entregar medicamentos

Actor: Cliente
Caso de uso: Administrar medicamentos

Actor: Mascota
Caso de uso: Recuperarse de la enfermedad

Actor: Veterinario
Caso de uso: Informar recuperación

Actor: Cliente
Caso de uso: Pagar consulta

Actor: Veterinario
Caso de uso: Aceptar pago
```