```swift
// Importación de librerías necesarias
import Foundation

// Definición de una clase llamada "Persona" con propiedades y métodos
class Persona {
    var nombre: String
    var apellido: String
    var edad: Int

    // Constructor de la clase Persona
    init(nombre: String, apellido: String, edad: Int) {
        self.nombre = nombre
        self.apellido = apellido
        self.edad = edad
    }

    // Método para obtener el nombre completo de la persona
    func obtenerNombreCompleto() -> String {
        return "\(nombre) \(apellido)"
    }

    // Método para obtener la edad de la persona
    func obtenerEdad() -> Int {
        return edad
    }
}

// Definición de una clase llamada "Vehiculo" con propiedades y métodos
class Vehiculo {
    var marca: String
    var modelo: String
    var anio: Int

    // Constructor de la clase Vehiculo
    init(marca: String, modelo: String, anio: Int) {
        self.marca = marca
        self.modelo = modelo
        self.anio = anio
    }

    // Método para obtener la descripción del vehículo
    func obtenerDescripcion() -> String {
        return "\(marca) \(modelo) (\(anio))"
    }
}

// Definición de una clase llamada "Viaje" con propiedades y métodos
class Viaje {
    var origen: String
    var destino: String
    var fecha: Date
    var hora: Date
    var vehiculo: Vehiculo
    var pasajeros: [Persona]

    // Constructor de la clase Viaje
    init(origen: String, destino: String, fecha: Date, hora: Date, vehiculo: Vehiculo, pasajeros: [Persona]) {
        self.origen = origen
        self.destino = destino
        self.fecha = fecha
        self.hora = hora
        self.vehiculo = vehiculo
        self.pasajeros = pasajeros
    }

    // Método para obtener la descripción del viaje
    func obtenerDescripcion() -> String {
        var descripcion = "Viaje de \(origen) a \(destino) el \(fecha) a las \(hora) en un \(vehiculo.obtenerDescripcion())\n"
        descripcion += "Pasajeros:\n"
        for pasajero in pasajeros {
            descripcion += "- \(pasajero.obtenerNombreCompleto())\n"
        }
        return descripcion
    }
}

// Creación de una instancia de la clase Persona
let persona1 = Persona(nombre: "Juan", apellido: "Pérez", edad: 25)

// Creación de una instancia de la clase Vehiculo
let vehiculo1 = Vehiculo(marca: "Toyota", modelo: "Camry", anio: 2020)

// Creación de una instancia de la clase Viaje
let viaje1 = Viaje(origen: "Madrid", destino: "Barcelona", fecha: Date(), hora: Date(), vehiculo: vehiculo1, pasajeros: [persona1])

// Impresión de la descripción del viaje
print(viaje1.obtenerDescripcion())
```

Este código crea tres clases: Persona, Vehiculo y Viaje. La clase Persona tiene propiedades para el nombre, apellido y edad de una persona, y métodos para obtener el nombre completo y la edad de la persona. La clase Vehiculo tiene propiedades para la marca, el modelo y el año de un vehículo, y un método para obtener la descripción del vehículo. La clase Viaje tiene propiedades para el origen, el destino, la fecha, la hora, el vehículo y los pasajeros del viaje, y un método para obtener la descripción del viaje.

El código también crea una instancia de la clase Persona, una instancia de la clase Vehiculo y una instancia de la clase Viaje. Por último, el código imprime la descripción del viaje.