```scala
// Explicación inicial:
// Este código es un programa complejo en Scala que implementa una aplicación simple de consola para gestionar productos y pedidos en una tienda en línea.

// Paso 1: Definir los datos del producto:
// Representamos los productos como clases de datos.

case class Producto(id: Int, nombre: String, descripcion: String, precio: Double)

// Paso 2: Definir los datos del pedido:
// Representamos los pedidos como clases de datos.

case class Pedido(id: Int, productos: List[Producto], cliente: String, direccion: String, total: Double)

// Paso 3: Definir la interfaz de la aplicación:
// Usamos la función principal `main` para ejecutar la aplicación.

object GestionInventario {

  // Función para imprimir una línea de separación en la consola.
  def imprimirLinea(): Unit = {
    println("-" * 50)
  }

  // Función para imprimir los detalles de un producto.
  def imprimirProducto(producto: Producto): Unit = {
    println(s"ID: ${producto.id}")
    println(s"Nombre: ${producto.nombre}")
    println(s"Descripción: ${producto.descripcion}")
    println(s"Precio: ${producto.precio}")
    println()
  }

  // Función para imprimir los detalles de un pedido.
  def imprimirPedido(pedido: Pedido): Unit = {
    println(s"ID: ${pedido.id}")
    println("Productos:")
    pedido.productos.foreach(producto => imprimirProducto(producto))
    println(s"Cliente: ${pedido.cliente}")
    println(s"Dirección: ${pedido.direccion}")
    println(s"Total: ${pedido.total}")
    println()
  }

  // Función para obtener la entrada del usuario.
  def obtenerEntrada(texto: String): String = {
    print(texto)
    scala.io.StdIn.readLine()
  }

  // Función para añadir un producto al inventario.
  def añadirProducto(productos: List[Producto]): List[Producto] = {
    val id = productos.lastOption.map(_.id + 1).getOrElse(1)
    val nombre = obtenerEntrada("Nombre del producto: ")
    val descripcion = obtenerEntrada("Descripción del producto: ")
    val precio = obtenerEntrada("Precio del producto: ").toDouble
    val nuevoProducto = Producto(id, nombre, descripcion, precio)
    productos :+ nuevoProducto
  }

  // Función para realizar un pedido.
  def realizarPedido(productos: List[Producto]): List[Pedido] = {
    val id = pedidos.lastOption.map(_.id + 1).getOrElse(1)
    val cliente = obtenerEntrada("Nombre del cliente: ")
    val direccion = obtenerEntrada("Dirección del cliente: ")
    val productosSeleccionados = obtenerEntrada("IDs de los productos deseados (separados por comas): ")
      .split(",").map(_.toInt).toList
    val productosPedido = productosSeleccionados.map(id => productos.find(_.id == id).get)
    val total = productosPedido.map(_.precio).sum
    val nuevoPedido = Pedido(id, productosPedido, cliente, direccion, total)
    pedidos :+ nuevoPedido
  }

  // Función para listar los productos en inventario.
  def listarProductos(productos: List[Producto]): Unit = {
    println("Listado de productos:")
    productos.foreach(producto => imprimirProducto(producto))
    imprimirLinea()
  }

  // Función para listar los pedidos realizados.
  def listarPedidos(pedidos: List[Pedido]): Unit = {
    println("Listado de pedidos:")
    pedidos.foreach(pedido => imprimirPedido(pedido))
    imprimirLinea()
  }

  // Función principal `main` que ejecuta la aplicación.
  def main(args: Array[String]): Unit = {
    var productos = List[Producto]()
    var pedidos = List[Pedido]()

    while (true) {
      // Mostrar el menú de opciones.
      println("Menú:")
      println("1. Añadir producto")
      println("2. Realizar pedido")
      println("3. Listar productos")
      println("4. Listar pedidos")
      println("5. Salir")

      // Obtener la opción elegida por el usuario.
      val opcion = obtenerEntrada("Elige una opción: ").toInt

      // Ejecutar la acción correspondiente a la opción elegida.
      opcion match {
        case 1 => productos = añadirProducto(productos)
        case 2 => pedidos = realizarPedido(productos)
        case 3 => listarProductos(productos)
        case 4 => listarPedidos(pedidos)
        case 5 => System.exit(0)
        case _ => println("Opción no válida. Por favor, elige una opción del 1 al 5.")
      }
    }
  }
}
```

Explicación del código:

* Definimos las clases de datos Producto y Pedido para representar los productos y pedidos en la tienda en línea.
* La función principal `main` ejecuta la aplicación y muestra un menú de opciones al usuario.
* Las funciones `añadirProducto()` y `realizarPedido()` permiten añadir productos al inventario y realizar pedidos, respectivamente.
* Las funciones `listarProductos()` y `listarPedidos()` permiten listar los productos en inventario y los pedidos realizados, respectivamente.
* La función `obtenerEntrada()` obtiene la entrada del usuario desde la consola.
* La función `imprimirLinea()` imprime una línea horizontal para separar las secciones del programa.
* La función `imprimirProducto()` imprime los detalles de un producto.
* La función `imprimirPedido()` imprime los detalles de un pedido.