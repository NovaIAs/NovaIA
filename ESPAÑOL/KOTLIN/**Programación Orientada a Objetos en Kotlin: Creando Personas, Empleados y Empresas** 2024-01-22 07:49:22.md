```kotlin
// Definimos una clase llamada "Persona" que representa a una persona con atributos y comportamientos.
class Persona {
    // Atributos de la clase "Persona".
    var nombre: String
    var edad: Int
    var direccion: String
    var telefono: String

    // Constructor de la clase "Persona".
    constructor(nombre: String, edad: Int, direccion: String, telefono: String) {
        this.nombre = nombre
        this.edad = edad
        this.direccion = direccion
        this.telefono = telefono
    }

    // Métodos de la clase "Persona".
    fun saludar() {
        println("Hola, mi nombre es $nombre y tengo $edad años.")
    }

    fun despedirse() {
        println("Adiós, espero verte pronto.")
    }
}

// Definimos una clase llamada "Empleado" que hereda de la clase "Persona" y tiene atributos y comportamientos específicos de un empleado.
class Empleado(nombre: String, edad: Int, direccion: String, telefono: String, var salario: Double, var puesto: String) : Persona(nombre, edad, direccion, telefono) {

    // Métodos de la clase "Empleado".
    fun trabajar() {
        println("Estoy trabajando duro.")
    }

    fun cobrarNomina() {
        println("He cobrado mi nómina. ¡Qué bien!")
    }
}

// Definimos una clase llamada "Empresa" que representa a una empresa con atributos y comportamientos.
class Empresa {
    // Atributos de la clase "Empresa".
    var nombre: String
    var direccion: String
    var telefono: String
    var empleados: MutableList<Empleado>

    // Constructor de la clase "Empresa".
    constructor(nombre: String, direccion: String, telefono: String) {
        this.nombre = nombre
        this.direccion = direccion
        this.telefono = telefono
        this.empleados = mutableListOf()
    }

    // Métodos de la clase "Empresa".
    fun contratarEmpleado(empleado: Empleado) {
        empleados.add(empleado)
    }

    fun despedirEmpleado(empleado: Empleado) {
        empleados.remove(empleado)
    }

    fun pagarNomina() {
        for (empleado in empleados) {
            empleado.cobrarNomina()
        }
    }
}

// Definimos una función principal para ejecutar el código.
fun main(args: Array<String>) {
    // Creamos una instancia de la clase "Empresa".
    val empresa = Empresa("Acme Corporation", "123 Main Street", "555-1212")

    // Creamos algunas instancias de la clase "Empleado".
    val empleado1 = Empleado("Juan García", 30, "456 Oak Street", "555-2323", 2000.0, "Ingeniero de software")
    val empleado2 = Empleado("María Pérez", 25, "789 Pine Street", "555-3434", 1500.0, "Contadora")
    val empleado3 = Empleado("Pedro Sánchez", 40, "1011 Elm Street", "555-4545", 2500.0, "Gerente de ventas")

    // Contratamos a los empleados en la empresa.
    empresa.contratarEmpleado(empleado1)
    empresa.contratarEmpleado(empleado2)
    empresa.contratarEmpleado(empleado3)

    // Pagamos la nómina a los empleados.
    empresa.pagarNomina()

    // Despedimos al empleado3.
    empresa.despedirEmpleado(empleado3)

    // Imprimimos la lista de empleados actuales de la empresa.
    for (empleado in empresa.empleados) {
        println(empleado.nombre)
    }
}
```

Este código define tres clases: "Persona", "Empleado" y "Empresa". La clase "Persona" representa a una persona con atributos como el nombre, la edad, la dirección y el teléfono. La clase "Empleado" hereda de la clase "Persona" y tiene atributos y comportamientos específicos de un empleado, como el salario y el puesto. La clase "Empresa" representa a una empresa con atributos como el nombre, la dirección y el teléfono. También tiene una lista de empleados.

El código también define una función principal que crea instancias de las clases "Persona", "Empleado" y "Empresa". La función principal contrata a los empleados en la empresa, paga la nómina y despide al empleado3. Finalmente, imprime la lista de empleados actuales de la empresa.

Este código es un ejemplo de cómo utilizar clases y herencia en Kotlin para crear un programa complejo.