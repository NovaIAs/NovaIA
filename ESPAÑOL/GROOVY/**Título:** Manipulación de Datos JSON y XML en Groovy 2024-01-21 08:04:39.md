```groovy
// Importar librerías necesarias
import groovy.json.JsonSlurper
import groovy.util.XmlSlurper

// Define una clase para representar los datos de un producto
class Producto {

    // Propiedades de la clase
    String nombre
    double precio
    String descripcion

    // Constructor de la clase
    Producto(String nombre, double precio, String descripcion) {
        this.nombre = nombre
        this.precio = precio
        this.descripcion = descripcion
    }

    // Método para obtener una representación JSON del producto
    String toJSON() {
        return new JsonBuilder(this).toPrettyString()
    }
}

// Define una clase para representar los datos de un pedido
class Pedido {

    // Propiedades de la clase
    List<Producto> productos
    String cliente
    String direccion

    // Constructor de la clase
    Pedido(List<Producto> productos, String cliente, String direccion) {
        this.productos = productos
        this.cliente = cliente
        this.direccion = direccion
    }

    // Método para obtener una representación JSON del pedido
    String toJSON() {
        return new JsonBuilder(this).toPrettyString()
    }
}

// Crea una lista de productos
List<Producto> productos = [
    new Producto("Producto 1", 10.0, "Este es el producto 1"),
    new Producto("Producto 2", 15.0, "Este es el producto 2"),
    new Producto("Producto 3", 20.0, "Este es el producto 3")
]

// Crea un pedido
Pedido pedido = new Pedido(productos, "Cliente 1", "Dirección 1")

// Convierte el pedido a JSON
String jsonPedido = pedido.toJSON()

// Imprime el JSON del pedido
println("JSON del pedido:")
println(jsonPedido)

// Parsear el JSON de un producto
String jsonProducto = """
{
    "nombre": "Producto 4",
    "precio": 25.0,
    "descripcion": "Este es el producto 4"
}
"""

// Crear un objeto Producto a partir del JSON
Producto producto = new JsonSlurper().parseText(jsonProducto)

// Imprimir los datos del producto
println("Datos del producto:")
println("Nombre: ${producto.nombre}")
println("Precio: ${producto.precio}")
println("Descripción: ${producto.descripcion}")

// Parsear el XML de un pedido
String xmlPedido = """
<pedido>
    <cliente>Cliente 2</cliente>
    <direccion>Dirección 2</direccion>
    <productos>
        <producto>
            <nombre>Producto 5</nombre>
            <precio>30.0</precio>
            <descripcion>Este es el producto 5</descripcion>
        </producto>
        <producto>
            <nombre>Producto 6</nombre>
            <precio>35.0</precio>
            <descripcion>Este es el producto 6</descripcion>
        </producto>
    </productos>
</pedido>
"""

// Crear un objeto Pedido a partir del XML
Pedido pedido2 = new XmlSlurper().parseText(xmlPedido)

// Imprimir los datos del pedido
println("Datos del pedido:")
println("Cliente: ${pedido2.cliente}")
println("Dirección: ${pedido2.direccion}")

for (Producto p : pedido2.productos) {
    println("Nombre: ${p.nombre}")
    println("Precio: ${p.precio}")
    println("Descripción: ${p.descripcion}")
}
```

**Explicación del código:**

* El código define dos clases, `Producto` y `Pedido`, para representar los datos de un producto y de un pedido, respectivamente.
* Crea una lista de productos y un pedido utilizando estas clases.
* Convierte el pedido a JSON utilizando el método `toJSON()` de la clase `Pedido`.
* Imprime el JSON del pedido en la consola.
* Parsear el JSON de un producto a partir de una cadena JSON y crear un objeto `Producto` a partir de él.
* Imprime los datos del producto en la consola.
* Parsear el XML de un pedido a partir de una cadena XML y crear un objeto `Pedido` a partir de él.
* Imprime los datos del pedido en la consola.

Este código es complejo y abarca una variedad de conceptos en Groovy, incluyendo clases, métodos, JSON y XML. Está diseñado para mostrar la potencia y versatilidad de Groovy para trabajar con diferentes tipos de datos y formatos.