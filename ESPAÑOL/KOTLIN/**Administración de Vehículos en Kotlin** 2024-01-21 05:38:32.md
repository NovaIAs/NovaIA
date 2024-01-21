```kotlin
// Definir una clase para representar un vehículo
class Vehículo(val tipo: String, val marca: String, val modelo: String, val año: Int) {

    // Definir una función para obtener la información del vehículo
    fun getInformacion(): String {
        return "Tipo: $tipo, Marca: $marca, Modelo: $modelo, Año: $año"
    }
}

// Definir una función para crear una lista de vehículos
fun crearListaVehiculos(): List<Vehículo> {
    return listOf(
        Vehículo("Coche", "Toyota", "Camry", 2023),
        Vehículo("Motocicleta", "Honda", "CB500X", 2022),
        Vehículo("Camión", "Ford", "F-150", 2021),
        Vehículo("Autobús", "Mercedes-Benz", "Sprinter", 2020),
        Vehículo("Bicicleta", "Specialized", "Rockhopper", 2019)
    )
}

// Definir una función para mostrar la información de una lista de vehículos
fun mostrarListaVehiculos(listaVehiculos: List<Vehículo>) {
    for (vehiculo in listaVehiculos) {
        println(vehiculo.getInformacion())
    }
}

// Definir una función para filtrar una lista de vehículos por tipo
fun filtrarVehiculosPorTipo(listaVehiculos: List<Vehículo>, tipo: String): List<Vehículo> {
    return listaVehiculos.filter { it.tipo == tipo }
}

// Definir una función para ordenar una lista de vehículos por año
fun ordenarVehiculosPorAño(listaVehiculos: List<Vehículo>): List<Vehículo> {
    return listaVehiculos.sortedBy { it.año }
}

// Crear una lista de vehículos
val listaVehiculos = crearListaVehiculos()

// Mostrar la información de la lista de vehículos
println("Lista de vehículos:")
mostrarListaVehiculos(listaVehiculos)

// Filtrar la lista de vehículos por tipo "Coche"
val listaCoches = filtrarVehiculosPorTipo(listaVehiculos, "Coche")

// Mostrar la información de la lista de coches
println("Lista de coches:")
mostrarListaVehiculos(listaCoches)

// Ordenar la lista de vehículos por año
val listaVehiculosOrdenadosPorAño = ordenarVehiculosPorAño(listaVehiculos)

// Mostrar la información de la lista de vehículos ordenados por año
println("Lista de vehículos ordenados por año:")
mostrarListaVehiculos(listaVehiculosOrdenadosPorAño)
```

Explicación del código:

* Se define una clase `Vehículo` con propiedades para representar el tipo, la marca, el modelo y el año del vehículo.
* Se define una función `crearListaVehiculos()` que crea una lista de vehículos.
* Se define una función `mostrarListaVehiculos()` que muestra la información de una lista de vehículos.
* Se define una función `filtrarVehiculosPorTipo()` que filtra una lista de vehículos por tipo.
* Se define una función `ordenarVehiculosPorAño()` que ordena una lista de vehículos por año.
* Se crea una lista de vehículos llamando a la función `crearListaVehiculos()`.
* Se muestra la información de la lista de vehículos llamando a la función `mostrarListaVehiculos()`.
* Se filtra la lista de vehículos por tipo "Coche" llamando a la función `filtrarVehiculosPorTipo()`.
* Se muestra la información de la lista de coches llamando a la función `mostrarListaVehiculos()`.
* Se ordena la lista de vehículos por año llamando a la función `ordenarVehiculosPorAño()`.
* Se muestra la información de la lista de vehículos ordenados por año llamando a la función `mostrarListaVehiculos()`.