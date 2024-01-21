```swift
import Foundation

// Definición de la clase Persona
class Persona {
    var nombre: String
    var edad: Int
    var ciudad: String

    init(nombre: String, edad: Int, ciudad: String) {
        self.nombre = nombre
        self.edad = edad
        self.ciudad = ciudad
    }

    func saludar() {
        print("Hola, mi nombre es \(nombre) y tengo \(edad) años. Soy de \(ciudad).")
    }
}

// Definición de la clase Empleado
class Empleado: Persona {
    var salario: Double

    init(nombre: String, edad: Int, ciudad: String, salario: Double) {
        self.salario = salario
        super.init(nombre: nombre, edad: edad, ciudad: ciudad)
    }

    override func saludar() {
        super.saludar()
        print("Soy empleado y gano \(salario) euros al mes.")
    }
}

// Definición de la clase Estudiante
class Estudiante: Persona {
    var carrera: String

    init(nombre: String, edad: Int, ciudad: String, carrera: String) {
        self.carrera = carrera
        super.init(nombre: nombre, edad: edad, ciudad: ciudad)
    }

    override func saludar() {
        super.saludar()
        print("Soy estudiante y estudio \(carrera).")
    }
}

// Creación de un objeto de tipo Persona
let persona = Persona(nombre: "Juan", edad: 25, ciudad: "Madrid")

// Creación de un objeto de tipo Empleado
let empleado = Empleado(nombre: "María", edad: 30, ciudad: "Barcelona", salario: 2000)

// Creación de un objeto de tipo Estudiante
let estudiante = Estudiante(nombre: "Pedro", edad: 20, ciudad: "Valencia", carrera: "Ingeniería Informática")

// Llamada al método saludar() para cada objeto
persona.saludar()
empleado.saludar()
estudiante.saludar()`enter code here``

```

Explicación del código:

1. Definición de la clase Persona: Esta clase representa a una persona con atributos como nombre, edad y ciudad. También se define un método saludar() que imprime un mensaje con los datos de la persona.
2. Definición de la clase Empleado: Esta clase hereda de la clase Persona y añade un atributo salario. También se redefine el método saludar() para incluir información sobre el salario del empleado.
3. Definición de la clase Estudiante: Esta clase hereda de la clase Persona y añade un atributo carrera. También se redefine el método saludar() para incluir información sobre la carrera del estudiante.
4. Creación de un objeto de tipo Persona: Se crea un objeto de tipo Persona llamado "persona" con los valores "Juan", 25 y "Madrid" para los atributos nombre, edad y ciudad respectivamente.
5. Creación de un objeto de tipo Empleado: Se crea un objeto de tipo Empleado llamado "empleado" con los valores "María", 30, "Barcelona" y 2000 para los atributos nombre, edad, ciudad y salario respectivamente.
6. Creación de un objeto de tipo Estudiante: Se crea un objeto de tipo Estudiante llamado "estudiante" con los valores "Pedro", 20, "Valencia" y "Ingeniería Informática" para los atributos nombre, edad, ciudad y carrera respectivamente.
7. Llamada al método saludar() para cada objeto: Se llama al método saludar() para cada uno de los objetos persona, empleado y estudiante. Esto imprime los mensajes "Hola, mi nombre es Juan y tengo 25 años. Soy de Madrid.", "Hola, mi nombre es María y tengo 30 años. Soy de Barcelona. Soy empleado y gano 2000 euros al mes." y "Hola, mi nombre es Pedro y tengo 20 años. Soy de Valencia. Soy estudiante y estudio Ingeniería Informática." respectivamente.