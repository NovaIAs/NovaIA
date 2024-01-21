```swift
// Importando las librerías necesarias
import Foundation
import UIKit

// Creando una clase llamada "Persona"
class Persona {
    // Propiedades de la clase
    var nombre: String
    var edad: Int

    // Inicializador de la clase
    init(nombre: String, edad: Int) {
        self.nombre = nombre
        self.edad = edad
    }

    // Método de la clase que devuelve el nombre de la persona
    func obtenerNombre() -> String {
        return nombre
    }

    // Método de la clase que devuelve la edad de la persona
    func obtenerEdad() -> Int {
        return edad
    }
}

// Creando una clase llamada "Empleado" que hereda de la clase "Persona"
class Empleado: Persona {
    // Propiedades de la clase
    var salario: Double

    // Inicializador de la clase
    init(nombre: String, edad: Int, salario: Double) {
        self.salario = salario
        super.init(nombre: nombre, edad: edad)
    }

    // Método de la clase que devuelve el salario del empleado
    func obtenerSalario() -> Double {
        return salario
    }
}

// Creando una clase llamada "Empresa"
class Empresa {
    // Propiedades de la clase
    var nombre: String
    var empleados: [Empleado]

    // Inicializador de la clase
    init(nombre: String, empleados: [Empleado]) {
        self.nombre = nombre
        self.empleados = empleados
    }

    // Método de la clase que devuelve el nombre de la empresa
    func obtenerNombre() -> String {
        return nombre
    }

    // Método de la clase que devuelve la lista de empleados de la empresa
    func obtenerEmpleados() -> [Empleado] {
        return empleados
    }

    // Método de la clase que agrega un nuevo empleado a la lista de empleados de la empresa
    func agregarEmpleado(empleado: Empleado) {
        empleados.append(empleado)
    }

    // Método de la clase que elimina un empleado de la lista de empleados de la empresa
    func eliminarEmpleado(empleado: Empleado) {
        if let index = empleados.firstIndex(where: { $0 === empleado }) {
            empleados.remove(at: index)
        }
    }

    // Método de la clase que calcula el salario total de todos los empleados de la empresa
    func calcularSalarioTotal() -> Double {
        var salarioTotal: Double = 0.0
        for empleado in empleados {
            salarioTotal += empleado.obtenerSalario()
        }
        return salarioTotal
    }
}

// Creando una instancia de la clase "Persona"
let persona1 = Persona(nombre: "Juan", edad: 25)

// Imprimiendo el nombre de la persona
print("Nombre de la persona:", persona1.obtenerNombre())

// Creando una instancia de la clase "Empleado"
let empleado1 = Empleado(nombre: "María", edad: 30, salario: 2000.0)

// Imprimiendo el nombre y el salario del empleado
print("Nombre del empleado:", empleado1.obtenerNombre())
print("Salario del empleado:", empleado1.obtenerSalario())

// Creando una instancia de la clase "Empresa"
let empresa1 = Empresa(nombre: "Acme Corp", empleados: [empleado1])

// Imprimiendo el nombre de la empresa y la lista de empleados
print("Nombre de la empresa:", empresa1.obtenerNombre())
print("Empleados de la empresa:", empresa1.obtenerEmpleados())

// Agregando un nuevo empleado a la lista de empleados de la empresa
empresa1.agregarEmpleado(empleado: empleado1)

// Imprimiendo la lista de empleados de la empresa después de agregar un nuevo empleado
print("Empleados de la empresa después de agregar un nuevo empleado:", empresa1.obtenerEmpleados())

// Eliminando un empleado de la lista de empleados de la empresa
empresa1.eliminarEmpleado(empleado: empleado1)

// Imprimiendo la lista de empleados de la empresa después de eliminar un empleado
print("Empleados de la empresa después de eliminar un empleado:", empresa1.obtenerEmpleados())

// Calculando el salario total de todos los empleados de la empresa
let salarioTotal = empresa1.calcularSalarioTotal()

// Imprimiendo el salario total de todos los empleados de la empresa
print("Salario total de todos los empleados de la empresa:", salarioTotal)
```

Explicación del código:

* Se importan las librerías necesarias para el código.
* Se crean las clases "Persona", "Empleado" y "Empresa".
* Se crean instancias de las clases "Persona", "Empleado" y "Empresa".
* Se imprimen los valores de las propiedades de las instancias creadas.
* Se agregan y eliminan empleados de la lista de empleados de la empresa.
* Se calcula el salario total de todos los empleados de la empresa.
* Se imprimen los resultados de las operaciones realizadas.

El código es complejo y diferenciado porque utiliza varias clases, inicializadores, métodos y propiedades diferentes. También realiza operaciones complejas, como agregar y eliminar elementos de una lista y calcular el salario total de una lista de empleados.