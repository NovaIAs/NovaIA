```groovy
import groovy.json.*

// Definición de la clase Producto
class Producto {
  String nombre
  BigDecimal precio
  Integer cantidad

  // Constructor
  Producto(String nombre, BigDecimal precio, Integer cantidad) {
    this.nombre = nombre
    this.precio = precio
    this.cantidad = cantidad
  }

  // Método para obtener el importe total del producto
  BigDecimal getImporteTotal() {
    precio * cantidad
  }

  // Método para obtener el producto en formato JSON
  String toJson() {
    def json = new JsonBuilder()
    json.producto {
      nombre nombre
      precio precio
      cantidad cantidad
    }
    json.toString()
  }
}

// Definición de la clase Factura
class Factura {
  Integer numero
  Date fecha
  String cliente
  List<Producto> productos

  // Constructor
  Factura(Integer numero, Date fecha, String cliente, List<Producto> productos) {
    this.numero = numero
    this.fecha = fecha
    this.cliente = cliente
    this.productos = productos
  }

  // Método para obtener el importe total de la factura
  BigDecimal getImporteTotal() {
    productos.collect { it.getImporteTotal() }.sum()
  }

  // Método para obtener la factura en formato JSON
  String toJson() {
    def json = new JsonBuilder()
    json.factura {
      numero numero
      fecha fecha
      cliente cliente
      productos productos.collect { it.toJson() }
    }
    json.toString()
  }
}

// Creación de una instancia de la clase Producto
def producto1 = new Producto("Producto 1", new BigDecimal("10.00"), 2)
def producto2 = new Producto("Producto 2", new BigDecimal("15.00"), 3)
def producto3 = new Producto("Producto 3", new BigDecimal("20.00"), 1)

// Creación de una instancia de la clase Factura
def factura = new Factura(1, new Date(), "Cliente 1", [producto1, producto2, producto3])

// Impresión de la factura en formato JSON
println factura.toJson()
```

Este código crea una clase Producto con los atributos nombre, precio y cantidad. También incluye un constructor para inicializar los atributos y un método para obtener el importe total del producto.

Además, se define una clase Factura con los atributos número, fecha, cliente y productos. El constructor inicializa los atributos y se incluye un método para obtener el importe total de la factura.

Luego se crean tres instancias de la clase Producto y se agregan a una lista.

A continuación, se crea una instancia de la clase Factura y se le agrega la lista de productos.

Por último, se imprime la factura en formato JSON utilizando la clase JsonBuilder de Groovy.