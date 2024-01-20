```groovy

import groovy.json.JsonSlurper
import java.time.LocalDate
import java.time.format.DateTimeFormatter

// Estos datos simulan la estructura de una lista de pedidos
def pedidos = [
    [id: 1, cliente: "Juan", fecha: "2023-04-25", productos: ["Manzana", "Pera", "Uva"]],
    [id: 2, cliente: "Pedro", fecha: "2023-03-08", productos: ["Naranja", "Platano", "Piña"]],
    [id: 3, cliente: "Luis", fecha: "2023-07-12", productos: ["Sandía", "Melón", "Mango"]]
]

// Creamos una función para calcular el total de cada pedido
def calcularTotal(pedido) {
    // Obtenemos los detalles del pedido
    def productos = pedido.productos
    def precios = [Manzana: 1.50, Pera: 2.00, Uva: 3.00, Naranja: 2.50, Platano: 1.75, Piña: 3.50, Sandía: 4.00, Melón: 3.00, Mango: 3.25]

    // Calculamos el precio total
    def total = 0
    productos.each { producto -> total += precios[producto] }

    // Retornamos el total
    return total
}

// Creamos una función para generar un informe en formato JSON
def generarInformeJSON() {
    def informe = []

    // Pasamos una lista de pedidos a la función para calcular el total
    pedidos.each { pedido ->
        // Obtenemos los detalles del pedido
        def id = pedido.id
        def cliente = pedido.cliente
        def fecha = pedido.fecha
        def productos = pedido.productos
        def total = calcularTotal(pedido)

        // Agregamos el pedido al informe
        informe.add([id: id, cliente: cliente, fecha: fecha, productos: productos, total: total])
    }

    // Retornamos el informe en formato JSON
    return new JsonSlurper().parseText(informe.toString())
}

// Creamos una función para generar un informe en formato CSV
def generarInformeCSV() {
    def informe = ""

    // Pasamos una lista de pedidos a la función para calcular el total
    pedidos.each { pedido ->
        // Obtenemos los detalles del pedido
        def id = pedido.id
        def cliente = pedido.cliente
        def fecha = pedido.fecha
        def productos = pedido.productos
        def total = calcularTotal(pedido)

        // Agregamos el pedido al informe
        informe += "$id,$cliente,$fecha,"
        productos.each { producto -> informe += "$producto," }
        informe += "$total\n"
    }

    // Retornamos el informe en formato CSV
    return informe
}

// Obtenemos el informe en formato JSON
def informeJSON = generarInformeJSON()

// Obtenemos el informe en formato CSV
def informeCSV = generarInformeCSV()

// Imprimimos el informe en consola
println(informeJSON)
println(informeCSV)

// Obtenemos la fecha actual
def fechaActual = LocalDate.now()

// Imprimimos la fecha actual en diferentes formatos
println("Fecha actual (formato ISO): $fechaActual")
println("Fecha actual (formato largo): ${fechaActual.format(DateTimeFormatter.ofPattern('dd/MM/yyyy'))}")
println("Fecha actual (formato corto): ${fechaActual.format(DateTimeFormatter.ofPattern('dd/MM/yy'))}")

// Invertimos una cadena de texto
def cadena = "Hola mundo!"
def cadenaInvertida = cadena.reverse()
println("Cadena original: $cadena")
println("Cadena invertida: $cadenaInvertida")

// Creamos un mapa con claves y valores
def mapa = [nombre: "Juan", edad: 30, ciudad: "Madrid"]

// Imprimimos el mapa
println(mapa)

// Accedemos a los valores del mapa
println("Nombre: ${mapa.nombre}")
println("Edad: ${mapa.edad}")
println("Ciudad: ${mapa.ciudad}")

// Creamos una lista con nombres
def nombres = ["Juan", "Pedro", "Luis"]

// Imprimimos la lista
println(nombres)

// Accedemos a los elementos de la lista
println("Primer nombre: ${nombres[0]}")
println("Segundo nombre: ${nombres[1]}")
println("Tercer nombre: ${nombres[2]}")

```

Explicación:

* El código define una lista de pedidos, cada pedido tiene un id, un cliente, una fecha y una lista de productos.
* Se crean dos funciones, una para calcular el total de cada pedido y otra para generar un informe en formato JSON o CSV.
* Se llama a las funciones para generar los informes y se imprimen en consola.
* Se obtienen la fecha actual y se imprime en diferentes formatos.
* Se invierte una cadena de texto y se imprime la cadena original y la cadena invertida.
* Se crea un mapa con claves y valores y se imprime el mapa.
* Se accede a los valores del mapa y se imprimen.
* Se crea una lista con nombres y se imprime la lista.
* Se accede a los elementos de la lista y se imprimen.