```
Sistema de Gestión de Recursos Humanos (RH)

Paquetes:

- Paquete RH:
  - Clase Empleado:
    - Atributos:
      - idEmpleado: int
      - nombre: string
      - apellido: string
      - cedula: string
      - correoElectronico: string
      - telefono: string
      - direccion: string
      - fechaNacimiento: date
      - fechaIngreso: date
      - fechaSalida: date (opcional)
      - cargo: string
      - departamento: string
      - salario: double
      - bonificaciones: double (opcional)
      - deducciones: double (opcional)
    - Métodos:
      - Constructor: inicializa los atributos de la clase.
      - Getters y setters: métodos para obtener y establecer valores de los atributos.
      - CalcularSalarioNeto: método que calcula el salario neto del empleado, restando las deducciones del salario bruto.
      - ToString: método que devuelve una cadena con la información del empleado.

- Paquete Nómina:
  - Clase Nomina:
    - Atributos:
      - idNomina: int
      - fechaPago: date
      - empleados: List<Empleado>
    - Métodos:
      - Constructor: inicializa los atributos de la clase.
      - Getters y setters: métodos para obtener y establecer valores de los atributos.
      - CalcularNomina: método que calcula la nómina de los empleados, sumando el salario neto de cada empleado.
      - ToString: método que devuelve una cadena con la información de la nómina.

- Paquete Interfaz de Usuario (UI):
  - Clase VentanaPrincipal:
    - Atributos:
      - empleados: List<Empleado>
      - nominas: List<Nomina>
    - Métodos:
      - Constructor: inicializa los atributos de la clase.
      - CargarEmpleados: método que carga la lista de empleados desde la base de datos.
      - CargarNominas: método que carga la lista de nóminas desde la base de datos.
      - MostrarEmpleados: método que muestra la lista de empleados en la interfaz de usuario.
      - MostrarNominas: método que muestra la lista de nóminas en la interfaz de usuario.
      - CrearEmpleado: método que crea un nuevo empleado y lo guarda en la base de datos.
      - EditarEmpleado: método que edita un empleado existente y lo guarda en la base de datos.
      - EliminarEmpleado: método que elimina un empleado existente de la base de datos.
      - CrearNomina: método que crea una nueva nómina y la guarda en la base de datos.
      - EditarNomina: método que edita una nómina existente y la guarda en la base de datos.
      - EliminarNomina: método que elimina una nómina existente de la base de datos.

- Paquete Base de Datos:
  - Clase ConexionBD:
    - Atributos:
      - conexion: Connection
    - Métodos:
      - Constructor: inicializa los atributos de la clase.
      - Conectar: método que establece una conexión con la base de datos.
      - Desconectar: método que cierra la conexión con la base de datos.
      - EjecutarConsulta: método que ejecuta una consulta SQL en la base de datos.
      - ObtenerResultados: método que obtiene los resultados de una consulta SQL.

Explicaciones:

- El paquete RH contiene las clases relacionadas con los empleados del sistema. La clase Empleado representa a los empleados, con sus atributos y métodos asociados.

- El paquete Nómina contiene las clases relacionadas con la nómina de los empleados. La clase Nómina representa la nómina de los empleados, con sus atributos y métodos asociados.

- El paquete Interfaz de Usuario (UI) contiene las clases relacionadas con la interfaz de usuario del sistema. La clase VentanaPrincipal representa la ventana principal del sistema, con sus atributos y métodos asociados.

- El paquete Base de Datos contiene las clases relacionadas con la base de datos del sistema. La clase ConexiónBD representa la conexión con la base de datos, con sus atributos y métodos asociados.

- El sistema funciona de la siguiente manera:

  - La clase VentanaPrincipal carga la lista de empleados y nóminas desde la base de datos y la muestra en la interfaz de usuario.

  - El usuario puede crear, editar o eliminar empleados y nóminas utilizando los métodos de la clase VentanaPrincipal.

  - Cuando el usuario crea una nómina, la clase Nómina calcula la nómina de los empleados y la guarda en la base de datos.

  - El usuario puede ver la nómina de los empleados en la interfaz de usuario.