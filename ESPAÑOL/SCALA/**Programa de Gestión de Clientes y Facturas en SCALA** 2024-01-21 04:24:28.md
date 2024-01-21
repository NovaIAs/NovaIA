**Programa de Gestión de Clientes y Facturas en SCALA**

```scala
import scala.collection.mutable.ListBuffer

class Cliente(val nombre: String, val apellidos: String, val dni: String) {
  var facturas: ListBuffer[Factura] = new ListBuffer[Factura]
}

class Factura(val numero: Int, val fecha: String, val cliente: Cliente, val lineasFactura: ListBuffer[LineaFactura]) {
  var total: Double = 0.0
  for (linea <- lineasFactura) {
    total += linea.importe
  }
}

class LineaFactura(val descripcion: String, val cantidad: Int, val precioUnidad: Double) {
  var importe: Double = cantidad * precioUnidad
}

object GestionClientesFacturas {
  val clientes = new ListBuffer[Cliente]
  val facturas = new ListBuffer[Factura]

  def main(args: Array[String]): Unit = {
    // Crear clientes
    val cliente1 = new Cliente("Juan", "García", "12345678A")
    val cliente2 = new Cliente("María", "Pérez", "87654321B")
    clientes += cliente1
    clientes += cliente2

    // Crear facturas
    val factura1 = new Factura(1, "2023-01-01", cliente1, new ListBuffer[LineaFactura])
    factura1.lineasFactura += new LineaFactura("Producto 1", 10, 10.0)
    factura1.lineasFactura += new LineaFactura("Producto 2", 5, 20.0)
    facturas += factura1

    val factura2 = new Factura(2, "2023-02-01", cliente2, new ListBuffer[LineaFactura])
    factura2.lineasFactura += new LineaFactura("Producto 3", 20, 15.0)
    factura2.lineasFactura += new LineaFactura("Producto 4", 10, 25.0)
    facturas += factura2

    // Mostrar información de los clientes y sus facturas
    for (cliente <- clientes) {
      println(s"Cliente: ${cliente.nombre} ${cliente.apellidos} (${cliente.dni})")
      for (factura <- cliente.facturas) {
        println(s"\tFactura: ${factura.numero} - ${factura.fecha} - Total: ${factura.total}")
      }
    }
  }
}
```

**Explicación del código:**

Este programa de gestión de clientes y facturas en SCALA tiene las siguientes características:

* **Clases:**

    * `Cliente`: Representa a un cliente con su nombre, apellidos y DNI. Tiene una lista de facturas asociadas.
    * `Factura`: Representa una factura con su número, fecha, cliente y una lista de líneas de factura. Tiene un atributo `total` que se calcula automáticamente sumando los importes de las líneas de factura.
    * `LineaFactura`: Representa una línea de factura con su descripción, cantidad y precio unitario. Tiene un atributo `importe` que se calcula automáticamente multiplicando la cantidad por el precio unitario.

* **Objetos:**

    * `GestionClientesFacturas`: Objeto principal del programa que contiene la lista de clientes y la lista de facturas.

* **Métodos:**

    * `main`: Método principal del programa que crea algunos clientes y facturas, y luego muestra la información de los clientes y sus facturas.

Este programa es un ejemplo de cómo crear clases, objetos y métodos en SCALA, y cómo utilizarlos para gestionar una lista de clientes y sus facturas.