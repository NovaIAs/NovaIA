```
Diagrama de Clases

<<Interface>> SistemaContable {
  + crearAsiento(asiento: Asiento): void
  + consultarAsiento(idAsiento: string): Asiento
  + modificarAsiento(asiento: Asiento): void
  + eliminarAsiento(idAsiento: string): void
}

Clase Asiento {
  - idAsiento: string
  - fecha: Date
  - concepto: string
  - importe: number
  - tipo: string
}

Clase LibroDiario {
  - asientos: List<Asiento>
  + registrarAsiento(asiento: Asiento): void
}

Clase LibroMayor {
  - cuentas: Map<string, Cuenta>
  + registrarAsiento(asiento: Asiento): void
}

Clase Cuenta {
  - codigo: string
  - nombre: string
  - saldo: number
  + cargar(importe: number): void
  + abonar(importe: number): void
}

Diagrama de Secuencia

1. Cliente invoca a la interfaz SistemaContable.crearAsiento(asiento).
2. SistemaContable.crearAsiento(asiento) valida el asiento y lo registra en el LibroDiario.
3. LibroDiario.registrarAsiento(asiento) añade el asiento a la lista de asientos.
4. SistemaContable.crearAsiento(asiento) registra el asiento en el LibroMayor.
5. LibroMayor.registrarAsiento(asiento) actualiza el saldo de las cuentas afectadas por el asiento.
6. SistemaContable.crearAsiento(asiento) devuelve el identificador del asiento al cliente.

Diagrama de Actividades

1. Cliente inicia el proceso de creación de un asiento.
2. SistemaContable valida el asiento.
3. LibroDiario registra el asiento.
4. LibroMayor registra el asiento.
5. SistemaContable devuelve el identificador del asiento al cliente.
6. Cliente finaliza el proceso de creación de un asiento.

Explicación

El código anterior es un ejemplo de cómo diseñar un sistema contable utilizando el lenguaje de modelado unificado (UML). El sistema está compuesto por las siguientes clases:

* SistemaContable: Esta clase es la interfaz principal del sistema. Proporciona los métodos para crear, consultar, modificar y eliminar asientos.
* Asiento: Esta clase representa un asiento contable. Contiene información como la fecha, el concepto, el importe y el tipo de asiento.
* LibroDiario: Esta clase representa el libro diario. Contiene una lista de todos los asientos registrados en el sistema.
* LibroMayor: Esta clase representa el libro mayor. Contiene un mapa de todas las cuentas contables del sistema, junto con sus saldos.
* Cuenta: Esta clase representa una cuenta contable. Contiene información como el código de la cuenta, el nombre de la cuenta y el saldo de la cuenta.

El código también incluye los siguientes diagramas:

* Diagrama de clases: Este diagrama muestra las clases del sistema y sus relaciones.
* Diagrama de secuencia: Este diagrama muestra la secuencia de mensajes que se intercambian entre los objetos del sistema cuando se crea un asiento.
* Diagrama de actividades: Este diagrama muestra el flujo de actividades que se producen cuando se crea un asiento.

Estos diagramas ayudan a comprender el diseño del sistema y su funcionamiento.