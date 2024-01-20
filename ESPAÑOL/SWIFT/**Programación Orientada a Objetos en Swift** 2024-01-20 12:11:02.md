```swift
// Crear una clase para representar una persona
class Persona {
    var nombre: String
    var apellidos: String
    var edad: Int

    init(nombre: String, apellidos: String, edad: Int) {
        self.nombre = nombre
        self.apellidos = apellidos
        self.edad = edad
    }

    // Declarar una función para mostrar la información de la persona
    func mostrarInformacion() {
        print("Nombre: \(nombre)")
        print("Apellidos: \(apellidos)")
        print("Edad: \(edad)")
    }
}

// Crear una clase para representar un empleado
class Empleado: Persona {
    var salario: Double
    var departamento: String

    init(nombre: String, apellidos: String, edad: Int, salario: Double, departamento: String) {
        super.init(nombre: nombre, apellidos: apellidos, edad: edad)
        self.salario = salario
        self.departamento = departamento
    }

    // Declarar una función para mostrar la información del empleado
    override func mostrarInformacion() {
        super.mostrarInformacion() // Llamar a la función mostrarInformacion() de la clase Persona
        print("Salario: \(salario)")
        print("Departamento: \(departamento)")
    }
}

// Crear una clase para representar un cliente
class Cliente: Persona {
    var dirección: String
    var teléfono: String

    init(nombre: String, apellidos: String, edad: Int, dirección: String, teléfono: String) {
        super.init(nombre: nombre, apellidos: apellidos, edad: edad)
        self.dirección = dirección
        self.teléfono = teléfono
    }

    // Declarar una función para mostrar la información del cliente
    override func mostrarInformacion() {
        super.mostrarInformacion() // Llamar a la función mostrarInformacion() de la clase Persona
        print("Dirección: \(dirección)")
        print("Teléfono: \(teléfono)")
    }
}

// Crear una lista de empleados
let empleados = [
    Empleado(nombre: "Juan", apellidos: "Pérez", edad: 25, salario: 1500.0, departamento: "Ventas"),
    Empleado(nombre: "María", apellidos: "García", edad: 30, salario: 2000.0, departamento: "Recursos Humanos"),
    Empleado(nombre: "Pedro", apellidos: "López", edad: 35, salario: 2500.0, departamento: "Contabilidad")
]

// Crear una lista de clientes
let clientes = [
    Cliente(nombre: "Ana", apellidos: "Fernández", edad: 20, dirección: "Calle Mayor, 123", teléfono: "912345678"),
    Cliente(nombre: "José", apellidos: "Rodríguez", edad: 25, dirección: "Calle Menor, 456", teléfono: "923456789"),
    Cliente(nombre: "Luis", apellidos: "Sánchez", edad: 30, dirección: "Calle Real, 789", teléfono: "934567890")
]

// Mostrar la información de los empleados y clientes
print("Empleados:")
for empleado in empleados {
    empleado.mostrarInformacion()
    print("---------------------")
}

print("Clientes:")
for cliente in clientes {
    cliente.mostrarInformacion()
    print("---------------------")
}
```

Explicación del código:

* Se definen tres clases: `Persona`, `Empleado` y `Cliente`. La clase `Persona` representa a una persona con nombre, apellidos y edad. La clase `Empleado` hereda de la clase `Persona` y añade información específica del empleado, como el salario y el departamento. La clase `Cliente` también hereda de la clase `Persona` y añade información específica del cliente, como la dirección y el teléfono.
* Se crean listas de empleados y clientes.
* Se muestran la información de los empleados y clientes.

El código anterior es un ejemplo de código complejo y diferenciado en SWIFT. Es difícil que se repita nuevamente, ya que es una combinación de varias clases, funciones y estructuras de datos.