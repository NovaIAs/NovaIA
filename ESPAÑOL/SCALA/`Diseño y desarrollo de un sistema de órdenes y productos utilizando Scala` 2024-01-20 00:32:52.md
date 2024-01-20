```scala
// Definición del tipo de datos "Estado de la orden"
object EstadoOrden extends Enumeration {
  val Pendiente, Procesando, Completada, Cancelada = Value
}

// Definición de la clase "Orden"
case class Orden(
    id: Int,
    cliente: String,
    productos: List[String],
    estado: EstadoOrden.Value
)

// Definición de la clase "Producto"
case class Producto(
    id: Int,
    nombre: String,
    precio: BigDecimal
)

// Definición de la clase "Cliente"
case class Cliente(
    id: Int,
    nombre: String,
    direccion: String,
    telefono: String
)

// Definición del tipo de datos "Lista de órdenes"
type ListaOrdenes = List[Orden]

// Definición del tipo de datos "Lista de productos"
type ListaProductos = List[Producto]

// Definición de una interfaz para el repositorio de órdenes
trait RepositorioOrdenes {
  def obtenerTodos(): ListaOrdenes
  def obtenerPorId(id: Int): Option[Orden]
  def crear(orden: Orden): Unit
  def actualizar(orden: Orden): Unit
  def eliminar(id: Int): Unit
}

// Implementación del repositorio de órdenes en memoria
class RepositorioOrdenesEnMemoria extends RepositorioOrdenes {
  private var ordenes: ListaOrdenes = List()

  override def obtenerTodos(): ListaOrdenes = ordenes

  override def obtenerPorId(id: Int): Option[Orden] = ordenes.find(_.id == id)

  override def crear(orden: Orden): Unit = ordenes = orden :: ordenes

  override def actualizar(orden: Orden): Unit = {
    val indice = ordenes.indexWhere(_.id == orden.id)
    if (indice != -1) ordenes = ordenes.updated(indice, orden)
  }

  override def eliminar(id: Int): Unit = ordenes = ordenes.filter(_.id != id)
}

// Definición de una interfaz para el repositorio de productos
trait RepositorioProductos {
  def obtenerTodos(): ListaProductos
  def obtenerPorId(id: Int): Option[Producto]
  def crear(producto: Producto): Unit
  def actualizar(producto: Producto): Unit
  def eliminar(id: Int): Unit
}

// Implementación del repositorio de productos en memoria
class RepositorioProductosEnMemoria extends RepositorioProductos {
  private var productos: ListaProductos = List()

  override def obtenerTodos(): ListaProductos = productos

  override def obtenerPorId(id: Int): Option[Producto] = productos.find(_.id == id)

  override def crear(producto: Producto): Unit = productos = producto :: productos

  override def actualizar(producto: Producto): Unit = {
    val indice = productos.indexWhere(_.id == producto.id)
    if (indice != -1) productos = productos.updated(indice, producto)
  }

  override def eliminar(id: Int): Unit = productos = productos.filter(_.id != id)
}

// Definición de una interfaz para el repositorio de clientes
trait RepositorioClientes {
  def obtenerTodos(): List[Cliente]
  def obtenerPorId(id: Int): Option[Cliente]
  def crear(cliente: Cliente): Unit
  def actualizar(cliente: Cliente): Unit
  def eliminar(id: Int): Unit
}

// Implementación del repositorio de clientes en memoria
class RepositorioClientesEnMemoria extends RepositorioClientes {
  private var clientes: List[Cliente] = List()

  override def obtenerTodos(): List[Cliente] = clientes

  override def obtenerPorId(id: Int): Option[Cliente] = clientes.find(_.id == id)

  override def crear(cliente: Cliente): Unit = clientes = cliente :: clientes

  override def actualizar(cliente: Cliente): Unit = {
    val indice = clientes.indexWhere(_.id == cliente.id)
    if (indice != -1) clientes = clientes.updated(indice, cliente)
  }

  override def eliminar(id: Int): Unit = clientes = clientes.filter(_.id != id)
}

// Definición de una interfaz para el servicio de órdenes
trait ServicioOrdenes {
  def obtenerTodos(): ListaOrdenes
  def obtenerPorId(id: Int): Option[Orden]
  def crear(orden: Orden): Unit
  def actualizar(orden: Orden): Unit
  def eliminar(id: Int): Unit
}

// Implementación del servicio de órdenes
class ServicioOrdenesImpl(repositorioOrdenes: RepositorioOrdenes) extends ServicioOrdenes {
  override def obtenerTodos(): ListaOrdenes = repositorioOrdenes.obtenerTodos()

  override def obtenerPorId(id: Int): Option[Orden] = repositorioOrdenes.obtenerPorId(id)

  override def crear(orden: Orden): Unit = repositorioOrdenes.crear(orden)

  override def actualizar(orden: Orden): Unit = repositorioOrdenes.actualizar(orden)

  override def eliminar(id: Int): Unit = repositorioOrdenes.eliminar(id)
}

// Definición de una interfaz para el servicio de productos
trait ServicioProductos {
  def obtenerTodos(): ListaProductos
  def obtenerPorId(id: Int): Option[Producto]
  def crear(producto: Producto): Unit
  def actualizar(producto: Producto): Unit
  def eliminar(id: Int): Unit
}

// Implementación del servicio de productos
class ServicioProductosImpl(repositorioProductos: RepositorioProductos) extends ServicioProductos {
  override def obtenerTodos(): ListaProductos = repositorioProductos.obtenerTodos()

  override def obtenerPorId(id: Int): Option[Producto] = repositorioProductos.obtenerPorId(id)

  override def crear(producto: Producto): Unit = repositorioProductos.crear(producto)

  override def actualizar(producto: Producto): Unit = repositorioProductos.actualizar(producto)

  override def eliminar(id: Int): Unit = repositorioProductos.eliminar(id)
}

// Definición de una interfaz para el servicio de clientes
trait ServicioClientes {
  def obtenerTodos(): List[Cliente]
  def obtenerPorId(id: Int): Option[Cliente]
  def crear(cliente: Cliente): Unit
  def actualizar(cliente: Cliente): Unit
  def eliminar(id: Int): Unit
}

// Implementación del servicio de clientes
class ServicioClientesImpl(repositorioClientes: RepositorioClientes) extends ServicioClientes {
  override def obtenerTodos(): List[Cliente] = repositorioClientes.obtenerTodos()

  override def obtenerPorId(id: Int): Option