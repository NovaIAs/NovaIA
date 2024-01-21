```kotlin
// Definición de una clase abstracta llamada "Vehiculo"
abstract class Vehiculo {

    // Propiedades comunes a todos los vehículos
    var marca: String
    var modelo: String
    var año: Int
    var propietario: String

    constructor(marca: String, modelo: String, año: Int, propietario: String) {
        this.marca = marca
        this.modelo = modelo
        this.año = año
        this.propietario = propietario
    }

    // Método abstracto para calcular el consumo de combustible
    abstract fun calcularConsumoCombustible(distancia: Double): Double

    // Método para mostrar la información del vehículo
    fun mostrarInformacion() {
        println("Marca: $marca")
        println("Modelo: $modelo")
        println("Año: $año")
        println("Propietario: $propietario")
    }
}

// Definición de una clase concreta llamada "Coche" que hereda de "Vehiculo"
class Coche(marca: String, modelo: String, año: Int, propietario: String) : Vehiculo(marca, modelo, año, propietario) {

    // Propiedades específicas de un coche
    var numPuertas: Int
    var tipoCombustible: String

    constructor(marca: String, modelo: String, año: Int, propietario: String, numPuertas: Int, tipoCombustible: String) : super(
        marca,
        modelo,
        año,
        propietario
    ) {
        this.numPuertas = numPuertas
        this.tipoCombustible = tipoCombustible
    }

    // Implementación del método abstracto para calcular el consumo de combustible
    override fun calcularConsumoCombustible(distancia: Double): Double {
        // Cálculo del consumo de combustible basado en el tipo de combustible y el número de puertas
        return if (tipoCombustible == "Gasolina" && numPuertas == 4) {
            distancia / 15.0 // 15 km/l para un coche de gasolina con 4 puertas
        } else if (tipoCombustible == "Diesel" && numPuertas == 5) {
            distancia / 20.0 // 20 km/l para un coche diesel con 5 puertas
        } else {
            distancia / 10.0 // Consumo por defecto para otros tipos de vehículos
        }
    }

    // Método para mostrar la información del coche
    override fun mostrarInformacion() {
        super.mostrarInformacion()
        println("Número de puertas: $numPuertas")
        println("Tipo de combustible: $tipoCombustible")
    }
}

// Definición de una clase concreta llamada "Camión" que hereda de "Vehiculo"
class Camión(marca: String, modelo: String, año: Int, propietario: String, capacidadCarga: Double) : Vehiculo(
    marca,
    modelo,
    año,
    propietario
) {

    // Propiedades específicas de un camión
    var capacidadCarga: Double

    constructor(marca: String, modelo: String, año: Int, propietario: String, capacidadCarga: Double) : super(
        marca,
        modelo,
        año,
        propietario
    ) {
        this.capacidadCarga = capacidadCarga
    }

    // Implementación del método abstracto para calcular el consumo de combustible
    override fun calcularConsumoCombustible(distancia: Double): Double {
        // Cálculo del consumo de combustible basado en la capacidad de carga
        return distancia / 10.0 * capacidadCarga // Consumo por cada 10 toneladas de carga
    }

    // Método para mostrar la información del camión
    override fun mostrarInformacion() {
        super.mostrarInformacion()
        println("Capacidad de carga: $capacidadCarga toneladas")
    }
}

// Definición de una clase concreta llamada "Moto" que hereda de "Vehiculo"
class Moto(marca: String, modelo: String, año: Int, propietario: String) : Vehiculo(marca, modelo, año, propietario) {

    // Propiedades específicas de una moto
    var cilindrada: Int

    constructor(marca: String, modelo: String, año: Int, propietario: String, cilindrada: Int) : super(
        marca,
        modelo,
        año,
        propietario
    ) {
        this.cilindrada = cilindrada