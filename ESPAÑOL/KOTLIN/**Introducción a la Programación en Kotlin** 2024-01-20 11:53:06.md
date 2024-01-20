```kotlin
// Clase Persona

class Persona(nombre: String, edad: Int) {
    var nombre: String = nombre
    var edad: Int = edad
}

// Clase Coche

class Coche(marca: String, modelo: String, año: Int) {
    var marca: String = marca
    var modelo: String = modelo
    var año: Int = año
}

// Clase Motor

class Motor(cilindrada: Int, potencia: Int) {
    var cilindrada: Int = cilindrada
    var potencia: Int = potencia
}

// Clase Función Principal

fun main(args: Array<String>) {
    // Crear objetos Persona, Coche y Motor

    var persona1 = Persona("Juan", 25)
    var coche1 = Coche("Toyota", "Corolla", 2018)
    var motor1 = Motor(1600, 110)

    // Imprimir información de los objetos

    println("Nombre de la persona: ${persona1.nombre}")
    println("Edad de la persona: ${persona1.edad}")

    println("Marca del coche: ${coche1.marca}")
    println("Modelo del coche: ${coche1.modelo}")
    println("Año del coche: ${coche1.año}")

    println("Cilindrada del motor: ${motor1.cilindrada} cm³")
    println("Potencia del motor: ${motor1.potencia} CV")

    // Condicional IF

    if (persona1.edad >= 18) {
        println("La persona es mayor de edad")
    } else {
        println("La persona es menor de edad")
    }

    // Bucle FOR

    for (i in 1..10) {
        println("Iteración $i")
    }

    // Función

    fun sumar(a: Int, b: Int): Int {
        return a + b
    }

    var resultado = sumar(5, 10)
    println("Resultado de la suma: $resultado")

    // Clase heredada

    class Camioneta(marca: String, modelo: String, año: Int, carga: Int) : Coche(marca, modelo, año) {
        var carga: Int = carga
    }

    var camioneta1 = Camioneta("Ford", "F-150", 2020, 1000)

    println("Marca de la camioneta: ${camioneta1.marca}")
    println("Modelo de la camioneta: ${camioneta1.modelo}")
    println("Año de la camioneta: ${camioneta1.año}")
    println("Carga de la camioneta: ${camioneta1.carga} kg")

    // Interfaces

    interface Vehiculo {
        fun acelerar()
        fun frenar()
    }

    class Autobus(marca: String, modelo: String, año: Int, asientos: Int) : Vehiculo {
        override fun acelerar() {
            println("El autobús acelera.")
        }

        override fun frenar() {
            println("El autobús frena.")
        }
    }

    var autobus1 = Autobus("Mercedes-Benz", "Citaro", 2022, 50)

    autobus1.acelerar()
    autobus1.frenar()

    // Excepciones

    try {
        var numero = Integer.parseInt("abc")
    } catch (e: NumberFormatException) {
        println("Se ha producido una excepción: ${e.message}")
    }
}
```

Explicación del código:

* Se definen las clases Persona, Coche, Motor y Función Principal.
* Se crean objetos de las clases Persona, Coche y Motor.
* Se imprimen los valores de los objetos en la consola.
* Se utiliza un condicional IF para determinar si una persona es mayor o menor de edad.
* Se utiliza un bucle FOR para iterar sobre una secuencia de números.
* Se define una función para sumar dos números y se llama desde la función principal.
* Se define una clase heredada Camioneta que hereda de la clase Coche.
* Se define una interfaz Vehiculo y se implementa en la clase Autobus.
* Se utiliza una excepción para manejar un error al intentar convertir una cadena de texto en un número.