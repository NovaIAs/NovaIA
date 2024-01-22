```kotlin
// Crear una clase Coche con las propiedades marca, modelo, año y color.
class Coche(val marca: String, val modelo: String, val año: Int, val color: String) {

    // Crear una función para obtener el nombre completo del coche.
    fun getNombreCompleto(): String {
        return "$marca $modelo $año"
    }

    // Crear una función para obtener el color del coche.
    fun getColor(): String {
        return color
    }

    // Crear una función para obtener el año del coche.
    fun getAño(): Int {
        return año
    }
}

// Crear una clase Taller con las propiedades nombre y dirección.
class Taller(val nombre: String, val dirección: String) {

    // Crear una función para obtener el nombre del taller.
    fun getNombre(): String {
        return nombre
    }

    // Crear una función para obtener la dirección del taller.
    fun getDirección(): String {
        return dirección
    }

    // Crear una función para reparar un coche.
    fun repararCoche(coche: Coche) {
        println("Reparando el coche ${coche.getNombreCompleto()}")
    }
}

// Crear una clase Cliente con las propiedades nombre, apellido y dirección.
class Cliente(val nombre: String, val apellido: String, val dirección: String) {

    // Crear una función para obtener el nombre completo del cliente.
    fun getNombreCompleto(): String {
        return "$nombre $apellido"
    }

    // Crear una función para obtener la dirección del cliente.
    fun getDirección(): String {
        return dirección
    }

    // Crear una función para llevar el coche al taller.
    fun llevarCocheAlTaller(coche: Coche, taller: Taller) {
        println("Llevando el coche ${coche.getNombreCompleto()} al taller ${taller.getNombre()}")
    }
}

// Crear un objeto de la clase Coche.
val coche = Coche("Toyota", "Corolla", 2020, "Rojo")

// Crear un objeto de la clase Taller.
val taller = Taller("Taller Mecánico", "Calle Mayor, 123")

// Crear un objeto de la clase Cliente.
val cliente = Cliente("Juan", "García", "Calle Menor, 321")

// Llamar a la función llevarCocheAlTaller del cliente.
cliente.llevarCocheAlTaller(coche, taller)

// Llamar a la función repararCoche del taller.
taller.repararCoche(coche)

// Imprimir el nombre completo del coche.
println("El coche es un ${coche.getNombreCompleto()}")

// Imprimir el color del coche.
println("El coche es de color ${coche.getColor()}")

// Imprimir el año del coche.
println("El coche es del año ${coche.getAño()}")

// Imprimir el nombre del taller.
println("El taller se llama ${taller.getNombre()}")

// Imprimir la dirección del taller.
println("El taller está en la dirección ${taller.getDirección()}")

// Imprimir el nombre completo del cliente.
println("El cliente se llama ${cliente.getNombreCompleto()}")

// Imprimir la dirección del cliente.
println("El cliente vive en la dirección ${cliente.getDirección()}")
```