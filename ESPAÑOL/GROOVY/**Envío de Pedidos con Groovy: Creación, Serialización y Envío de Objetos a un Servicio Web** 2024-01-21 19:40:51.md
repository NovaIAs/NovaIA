```groovy
// Importamos las librerías necesarias
import groovy.json.JsonSlurper
import groovy.transform.ToString

// Definimos una clase para representar un producto
@ToString
class Producto {
    String nombre
    double precio
}

// Definimos una clase para representar un pedido
@ToString
class Pedido {
    String cliente
    List<Producto> productos
    double total
}

// Definimos una clase para representar un cliente
@ToString
class Cliente {
    String nombre
    String dirección
    String teléfono
}

// Creamos un cliente
Cliente cliente = new Cliente(nombre: "Juan Pérez", dirección: "Calle Mayor 123", teléfono: "123456789")

// Creamos una lista de productos
List<Producto> productos = [
    new Producto(nombre: "Manzanas", precio: 1.5),
    new Producto(nombre: "Peras", precio: 2.0),
    new Producto(nombre: "Plátanos", precio: 1.2)
]

// Creamos un pedido
Pedido pedido = new Pedido(cliente: cliente, productos: productos)

// Calculamos el total del pedido
pedido.total = productos.sum({ producto -> producto.precio })

// Imprimimos el pedido
println pedido

// Serializamos el pedido en formato JSON
String json = new JsonSlurper().encode(pedido)

// Enviamos el pedido a un servicio web
URL url = new URL("http://localhost:8080/pedidos")
HttpURLConnection connection = (HttpURLConnection) url.openConnection()
connection.setRequestMethod("POST")
connection.setRequestProperty("Content-Type", "application/json")
connection.doOutput = true
connection.outputStream.write(json.bytes)
connection.connect()

// Leemos la respuesta del servicio web
String respuesta = connection.inputStream.text

// Imprimimos la respuesta
println respuesta
```

Este código crea un objeto `Cliente`, una lista de objetos `Producto`, un objeto `Pedido` y lo serializa en formato JSON. Luego, envía el pedido a un servicio web mediante una petición HTTP POST y lee la respuesta del servicio web.

El código está bien documentado y utiliza características avanzadas de Groovy, como las clases `@ToString` y la serialización JSON.