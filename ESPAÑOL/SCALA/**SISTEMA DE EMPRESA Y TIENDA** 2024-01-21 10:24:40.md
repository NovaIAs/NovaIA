```scala
// Definición de la clase "Persona"
class Persona(val nombre: String, val edad: Int) {

    // Método "hablar"
    def hablar(): Unit = {
        println(s"Hola, mi nombre es $nombre y tengo $edad años de edad.")
    }
}

// Definición de la clase "Empleado" que hereda de la clase "Persona"
class Empleado(nombre: String, edad: Int, val puesto: String, val salario: Double) extends Persona(nombre, edad) {

    // Método "trabajar"
    def trabajar(): Unit = {
        println(s"$nombre está trabajando como $puesto y gana $salario euros.")
    }
}

// Definición de la clase "Cliente"
class Cliente(nombre: String, edad: Int, val dni: String) extends Persona(nombre, edad) {

    // Método "comprar"
    def comprar(): Unit = {
        println(s"$nombre está comprando en la tienda.")
    }
}

// Definición de la clase "Empresa"
class Empresa(val nombre: String, val direccion: String, val empleados: List[Empleado]) {

    // Método "contratar"
    def contratar(empleado: Empleado): Unit = {
        empleados += empleado
    }

    // Método "despedir"
    def despedir(empleado: Empleado): Unit = {
        empleados -= empleado
    }

    // Método "pagarSalarios"
    def pagarSalarios(): Unit = {
        empleados.foreach(empleado => empleado.trabajar())
    }
}

// Definición de la clase "Tienda"
class Tienda(val nombre: String, val direccion: String, val clientes: List[Cliente]) {

    // Método "vender"
    def vender(cliente: Cliente): Unit = {
        clientes += cliente
    }

    // Método "atenderClientes"
    def atenderClientes(): Unit = {
        clientes.foreach(cliente => cliente.comprar())
    }
}

// Creamos una empresa
val empresa = new Empresa("Acme Corporation", "Calle Mayor, 123", List())

// Creamos varios empleados
val empleado1 = new Empleado("Juan", 30, "Ingeniero de software", 2000.0)
val empleado2 = new Empleado("María", 25, "Contable", 1500.0)
val empleado3 = new Empleado("Pedro", 40, "Director de marketing", 3000.0)

// Contratamos a los empleados
empresa.contratar(empleado1)
empresa.contratar(empleado2)
empresa.contratar(empleado3)

// Creamos una tienda
val tienda = new Tienda("La tienda de la esquina", "Calle Menor, 234", List())

// Creamos varios clientes
val cliente1 = new Cliente("Ana", 20, "12345678A")
val cliente2 = new Cliente("Luis", 35, "23456789B")
val cliente3 = new Cliente("Carmen", 45, "34567890C")

// Vendemos a los clientes
tienda.vender(cliente1)
tienda.vender(cliente2)
tienda.vender(cliente3)

// Pagamos los salarios de los empleados
empresa.pagarSalarios()

// Atendemos a los clientes
tienda.atenderClientes()
```

Explicación del código:

* Definimos las clases `Persona`, `Empleado`, `Cliente`, `Empresa` y `Tienda`.
* Creamos objetos de las clases `Persona`, `Empleado`, `Cliente`, `Empresa` y `Tienda`.
* Llamamos a los métodos de los objetos para realizar acciones como contratar empleados, despedir empleados, pagar salarios, vender productos y atender clientes.

El código anterior es un ejemplo de un programa complejo en Scala que realiza varias tareas diferentes. El código está bien organizado y es fácil de leer y entender.