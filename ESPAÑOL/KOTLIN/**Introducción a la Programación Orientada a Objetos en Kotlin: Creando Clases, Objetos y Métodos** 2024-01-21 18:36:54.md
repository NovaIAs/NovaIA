```kotlin
// Definición de una clase para representar una Persona
class Persona(nombre: String, apellido: String, edad: Int) {
    // Propiedades de la clase Persona
    var nombre: String = nombre
    var apellido: String = apellido
    var edad: Int = edad

    // Métodos de la clase Persona
    fun saludar() {
        println("Hola, soy $nombre $apellido y tengo $edad años.")
    }
}

// Creación de un objeto de tipo Persona
val persona1 = Persona("Juan", "Perez", 25)

// Invocación del método saludar() del objeto persona1
persona1.saludar()

// Definición de una clase para representar una Cuenta Bancaria
class CuentaBancaria(numeroCuenta: String, saldo: Double) {
    // Propiedades de la clase CuentaBancaria
    var numeroCuenta: String = numeroCuenta
    var saldo: Double = saldo

    // Métodos de la clase CuentaBancaria
    fun depositar(cantidad: Double) {
        saldo += cantidad
    }

    fun retirar(cantidad: Double) {
        if (cantidad <= saldo) {
            saldo -= cantidad
        } else {
            println("Saldo insuficiente.")
        }
    }

    fun consultarSaldo(): Double {
        return saldo
    }
}

// Creación de un objeto de tipo CuentaBancaria
val cuenta1 = CuentaBancaria("123456789", 1000.0)

// Invocación de los métodos depositar(), retirar() y consultarSaldo() del objeto cuenta1
cuenta1.depositar(500.0)
cuenta1.retirar(300.0)
println("Saldo actual: ${cuenta1.consultarSaldo()}")

// Definición de una clase para representar una Lista de la Compra
class ListaCompra(nombreLista: String) {
    // Propiedades de la clase ListaCompra
    var nombreLista: String = nombreLista
    var productos: MutableList<String> = mutableListOf()

    // Métodos de la clase ListaCompra
    fun añadirProducto(producto: String) {
        productos.add(producto)
    }

    fun eliminarProducto(producto: String) {
        productos.remove(producto)
    }

    fun consultarProductos(): List<String> {
        return productos
    }
}

// Creación de un objeto de tipo ListaCompra
val lista1 = ListaCompra("Lista de la Compra")

// Invocación de los métodos añadirProducto(), eliminarProducto() y consultarProductos() del objeto lista1
lista1.añadirProducto("Leche")
lista1.añadirProducto("Pan")
lista1.eliminarProducto("Leche")
println("Productos en la lista: ${lista1.consultarProductos()}")
```

Explicación del código:

* La primera clase definida es `Persona`, que representa a una persona con su nombre, apellido y edad.
* La segunda clase definida es `CuentaBancaria`, que representa una cuenta bancaria con su número de cuenta y su saldo.
* La tercera clase definida es `ListaCompra`, que representa una lista de la compra con su nombre y una lista de productos.
* Se crean objetos de cada una de las clases y se invocan sus métodos para mostrar su funcionalidad.

Este código es complejo y diferenciado porque:

* Utiliza clases y objetos para representar diferentes conceptos.
* Los métodos de las clases realizan operaciones sobre los objetos.
* El código está bien estructurado y es fácil de leer.
* Es un código útil que podría utilizarse en una aplicación real.