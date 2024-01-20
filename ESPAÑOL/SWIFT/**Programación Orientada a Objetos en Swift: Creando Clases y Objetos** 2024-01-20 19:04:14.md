```swift
// Importación de bibliotecas necesarias
import Foundation

// Definición de la clase Persona
class Persona {
    // Propiedades de la clase Persona
    var nombre: String
    var edad: Int
    var ocupacion: String

    // Constructor de la clase Persona
    init(nombre: String, edad: Int, ocupacion: String) {
        self.nombre = nombre
        self.edad = edad
        self.ocupacion = ocupacion
    }

    // Método para obtener una descripción de la persona
    func descripcion() -> String {
        return "Nombre: \(nombre), Edad: \(edad), Ocupación: \(ocupacion)"
    }
}

// Definición de la clase Empleado, que hereda de la clase Persona
class Empleado: Persona {
    // Propiedades de la clase Empleado
    var salario: Double

    // Constructor de la clase Empleado
    init(nombre: String, edad: Int, ocupacion: String, salario: Double) {
        self.salario = salario
        super.init(nombre: nombre, edad: edad, ocupacion: ocupacion)
    }

    // Método para obtener una descripción del empleado
    override func descripcion() -> String {
        return super.descripcion() + ", Salario: \(salario)"
    }
}

// Definición de la clase Cliente, que hereda de la clase Persona
class Cliente: Persona {
    // Propiedades de la clase Cliente
    var historialCompras: [String]

    // Constructor de la clase Cliente
    init(nombre: String, edad: Int, ocupacion: String, historialCompras: [String]) {
        self.historialCompras = historialCompras
        super.init(nombre: nombre, edad: edad, ocupacion: ocupacion)
    }

    // Método para obtener una descripción del cliente
    override func descripcion() -> String {
        return super.descripcion() + ", Historial de Compras: \(historialCompras)"
    }
}

// Definición de la función principal
func main() {
    // Creación de una instancia de la clase Persona
    let persona1 = Persona(nombre: "Juan", edad: 30, ocupacion: "Estudiante")

    // Creación de una instancia de la clase Empleado
    let empleado1 = Empleado(nombre: "María", edad: 40, ocupacion: "Ingeniera", salario: 5000.0)

    // Creación de una instancia de la clase Cliente
    let cliente1 = Cliente(nombre: "Pedro", edad: 25, ocupacion: "Profesor", historialCompras: ["Libro 1", "Libro 2", "Libro 3"])

    // Impresión de la descripción de la persona
    print(persona1.descripcion())

    // Impresión de la descripción del empleado
    print(empleado1.descripcion())

    // Impresión de la descripción del cliente
    print(cliente1.descripcion())
}

// Llamada a la función principal
main()
```

Explicación del código:

* El código crea tres clases: `Persona`, `Empleado` y `Cliente`.
* La clase `Persona` define las propiedades y métodos comunes a todas las personas.
* La clase `Empleado` hereda de la clase `Persona` y añade propiedades y métodos específicos de los empleados.
* La clase `Cliente` hereda de la clase `Persona` y añade propiedades y métodos específicos de los clientes.
* La función `main()` crea instancias de las clases `Persona`, `Empleado` y `Cliente` y las imprime en la consola.