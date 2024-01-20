```kotlin

// Clase Principal - Punto de entrada de la Aplicación.
class Aplicacion {

    // Función main - Esta función es el punto de entrada de la aplicación.
    fun main(args: Array<String>) {

        // Se crea un conductor.
        val conductor = Conductor("Juan", "Perez", 30)

        // Se crea un vehículo.
        val vehiculo = Vehiculo("Toyota", "Camry", 2020)

        // Se crea un viaje.
        val viaje = Viaje(conductor, vehiculo, "Origen", "Destino", 100)

        // Se inicia el viaje.
        viaje.iniciar()

        // Se finaliza el viaje.
        viaje.finalizar()

        // Se imprime el costo del viaje.
        println("Costo del Viaje: ${viaje.costo}")

    }

}

// Clase Conductor - Representa a un conductor.
class Conductor(nombre: String, apellido: String, edad: Int) {

    // Propiedades del conductor.
    val nombre: String
    val apellido: String
    val edad: Int

    // Constructor del conductor.
    init {
        this.nombre = nombre
        this.apellido = apellido
        this.edad = edad
    }

}

// Clase Vehiculo - Representa a un vehículo.
class Vehiculo(marca: String, modelo: String, año: Int) {

    // Propiedades del vehículo.
    val marca: String
    val modelo: String
    val año: Int

    // Constructor del vehículo.
    init {
        this.marca = marca
        this.modelo = modelo
        this.año = año
    }

}

// Clase Viaje - Representa a un viaje.
class Viaje(val conductor: Conductor, val vehiculo: Vehiculo, val origen: String, val destino: String, val distancia: Double) {

    // Propiedades del viaje.
    val costo: Double

    // Constructor del viaje.
    init {

        // Se calcula el costo del viaje.
        this.costo = distancia * 0.1

    }

    // Función para iniciar el viaje.
    fun iniciar() {

        // Se imprime un mensaje de inicio del viaje.
        println("Viaje iniciado desde $origen hacia $destino.")

    }

    // Función para finalizar el viaje.
    fun finalizar() {

        // Se imprime un mensaje de finalización del viaje.
        println("Viaje finalizado en $destino.")

    }

}

```

**Explicación del Código:**

* La clase `Aplicacion` es la clase principal de la aplicación. Contiene la función `main`, que es el punto de entrada de la aplicación.
* La clase `Conductor` representa a un conductor. Tiene propiedades como el nombre, el apellido y la edad.
* La clase `Vehiculo` representa a un vehículo. Tiene propiedades como la marca, el modelo y el año.
* La clase `Viaje` representa a un viaje. Tiene propiedades como el conductor, el vehículo, el origen, el destino y la distancia.
* La función `iniciar` de la clase `Viaje` se utiliza para iniciar el viaje.
* La función `finalizar` de la clase `Viaje` se utiliza para finalizar el viaje.
* El costo del viaje se calcula en el constructor de la clase `Viaje`.

Este código es un ejemplo de una aplicación simple de viajes. Se puede utilizar para crear una aplicación más compleja que incluya características como la reserva de viajes, el seguimiento de los viajes y el pago de los viajes.