```
Diagrama de Clases

**Package:** Sistema de Gestión de Recursos Humanos

**Clase:** Empleado

* Atributos:
    * IdEmpleado
    * Nombre
    * Apellido
    * CorreoElectronico
    * Teléfono
* Métodos:
    * ObtenerIdEmpleado()
    * ObtenerNombre()
    * ObtenerApellido()
    * ObtenerCorreoElectronico()
    * ObtenerTeléfono()
    * EstablecerIdEmpleado(IdEmpleado)
    * EstablecerNombre(Nombre)
    * EstablecerApellido(Apellido)
    * EstablecerCorreoElectronico(CorreoElectronico)
    * EstablecerTeléfono(Teléfono)

**Clase:** Departamento

* Atributos:
    * IdDepartamento
    * Nombre
    * Ubicación
* Métodos:
    * ObtenerIdDepartamento()
    * ObtenerNombre()
    * ObtenerUbicación()
    * EstablecerIdDepartamento(IdDepartamento)
    * EstablecerNombre(Nombre)
    * EstablecerUbicación(Ubicación)

**Clase:** Puesto

* Atributos:
    * IdPuesto
    * Nombre
    * Descripción
* Métodos:
    * ObtenerIdPuesto()
    * ObtenerNombre()
    * ObtenerDescripción()
    * EstablecerIdPuesto(IdPuesto)
    * EstablecerNombre(Nombre)
    * EstablecerDescripción(Descripción)

**Clase:** Nómina

* Atributos:
    * IdNómina
    * FechaPago
    * MontoPago
* Métodos:
    * ObtenerIdNómina()
    * ObtenerFechaPago()
    * ObtenerMontoPago()
    * EstablecerIdNómina(IdNómina)
    * EstablecerFechaPago(FechaPago)
    * EstablecerMontoPago(MontoPago)

**Clase:** Contrato

* Atributos:
    * IdContrato
    * FechaInicio
    * FechaFin
    * TipoContrato
* Métodos:
    * ObtenerIdContrato()
    * ObtenerFechaInicio()
    * ObtenerFechaFin()
    * ObtenerTipoContrato()
    * EstablecerIdContrato(IdContrato)
    * EstablecerFechaInicio(FechaInicio)
    * EstablecerFechaFin(FechaFin)
    * EstablecerTipoContrato(TipoContrato)

**Clase:** Permiso

* Atributos:
    * IdPermiso
    * FechaInicio
    * FechaFin
    * TipoPermiso
* Métodos:
    * ObtenerIdPermiso()
    * ObtenerFechaInicio()
    * ObtenerFechaFin()
    * ObtenerTipoPermiso()
    * EstablecerIdPermiso(IdPermiso)
    * EstablecerFechaInicio(FechaInicio)
    * EstablecerFechaFin(FechaFin)
    * EstablecerTipoPermiso(TipoPermiso)

**Clase:** Vacaciones

* Atributos:
    * IdVacaciones
    * FechaInicio
    * FechaFin
* Métodos:
    * ObtenerIdVacaciones()
    * ObtenerFechaInicio()
    * ObtenerFechaFin()
    * EstablecerIdVacaciones(IdVacaciones)
    * EstablecerFechaInicio(FechaInicio)
    * EstablecerFechaFin(FechaFin)

Diagrama de Casos de Uso

**Caso de Uso:** Registrar Nuevo Empleado

* Actores:
    * Administrador de Recursos Humanos
* Pasos:
    1. El administrador de recursos humanos inicia sesión en el sistema.
    2. El administrador de recursos humanos selecciona la opción "Registrar Nuevo Empleado".
    3. El administrador de recursos humanos ingresa los datos del nuevo empleado.
    4. El sistema valida los datos ingresados.
    5. El sistema guarda los datos del nuevo empleado.
    6. El sistema genera un mensaje de confirmación.

**Caso de Uso:** Calcular Nómina

* Actores:
    * Administrador de Recursos Humanos
* Pasos:
    1. El administrador de recursos humanos inicia sesión en el sistema.
    2. El administrador de recursos humanos selecciona la opción "Calcular Nómina".
    3. El sistema solicita el período de nómina.
    4. El administrador de recursos humanos ingresa el período de nómina.
    5. El sistema calcula la nómina de todos los empleados.
    6. El sistema genera un reporte de nómina.

**Diagrama de Secuencia:** Registrar Nuevo Empleado

* Objetos:
    * Administrador de Recursos Humanos
    * Sistema
* Mensajes:
    1. Administrador de Recursos Humanos -> Sistema: Iniciar sesión
    2. Sistema -> Administrador de Recursos Humanos: Mostrar pantalla de inicio de sesión
    3. Administrador de Recursos Humanos -> Sistema: Ingresar usuario y contraseña
    4. Sistema -> Administrador de Recursos Humanos: Validar usuario y contraseña
    5. Sistema -> Administrador de Recursos Humanos: Mostrar pantalla de registro de nuevo empleado
    6. Administrador de Recursos Humanos -> Sistema: Ingresar datos del nuevo empleado
    7. Sistema -> Administrador de Recursos Humanos: Validar datos ingresados
    8. Sistema -> Administrador de Recursos Humanos: Guardar datos del nuevo empleado
    9. Sistema -> Administrador de Recursos Humanos: Generar mensaje de confirmación

Explicación del Código:
El código anterior es un ejemplo de un modelo UML para un sistema de gestión de recursos humanos. El modelo incluye un diagrama de clases, un diagrama de casos de uso y un diagrama de secuencia.

El diagrama de clases define las clases del sistema y sus relaciones. Las clases son: Empleado, Departamento, Puesto