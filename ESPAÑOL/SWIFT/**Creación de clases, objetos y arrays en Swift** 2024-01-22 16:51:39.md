```swift
import Foundation

// DEFINIR UNA CLASE LLAMADA "Persona" CON DOS PROPIEDADES: "nombre" Y "edad".
class Persona {
    var nombre: String
    var edad: Int

    // INICIALIZAR LA CLASE "Persona" CON UN NOMBRE Y UNA EDAD.
    init(nombre: String, edad: Int) {
        self.nombre = nombre
        self.edad = edad
    }

    // DEFINIR UN MÉTODO LLAMADO "saludar" QUE IMPRIMA UN MENSAJE DE SALUDO CON EL NOMBRE DE LA PERSONA.
    func saludar() {
        print("Hola, mi nombre es \(nombre) y tengo \(edad) años.")
    }
}

// DEFINIR UNA CLASE LLAMADA "Coche" CON TRES PROPIEDADES: "marca", "modelo" Y "año".
class Coche {
    var marca: String
    var modelo: String
    var año: Int

    // INICIALIZAR LA CLASE "Coche" CON UNA MARCA, UN MODELO Y UN AÑO.
    init(marca: String, modelo: String, año: Int) {
        self.marca = marca
        self.modelo = modelo
        self.año = año
    }

    // DEFINIR UN MÉTODO LLAMADO "arrancar" QUE IMPRIMA UN MENSAJE DE ARRANQUE CON LA MARCA Y EL MODELO DEL COCHE.
    func arrancar() {
        print("El coche \(marca) \(modelo) ha arrancado.")
    }
}

// DEFINIR UNA CLASE LLAMADA "Casa" CON CUATRO PROPIEDADES: "dirección", "número", "habitaciones" Y "baños".
class Casa {
    var dirección: String
    var número: String
    var habitaciones: Int
    var baños: Int

    // INICIALIZAR LA CLASE "Casa" CON UNA DIRECCIÓN, UN NÚMERO, UN NÚMERO DE HABITACIONES Y UN NÚMERO DE BAÑOS.
    init(dirección: String, número: String, habitaciones: Int, baños: Int) {
        self.dirección = dirección
        self.número = número
        self.habitaciones = habitaciones
        self.baños = baños
    }

    // DEFINIR UN MÉTODO LLAMADO "abrirPuerta" QUE IMPRIMA UN MENSAJE DE APERTURA DE PUERTA CON LA DIRECCIÓN Y EL NÚMERO DE LA CASA.
    func abrirPuerta() {
        print("La puerta de la casa ubicada en \(dirección) \(número) se ha abierto.")
    }
}

// CREAR UN ARRAY DE PERSONAS.
let personas = [
    Persona(nombre: "Juan", edad: 20),
    Persona(nombre: "María", edad: 25),
    Persona(nombre: "Pedro", edad: 30)
]

// CREAR UN ARRAY DE COCHES.
let coches = [
    Coche(marca: "Toyota", modelo: "Corolla", año: 2022),
    Coche(marca: "Honda", modelo: "Civic", año: 2023),
    Coche(marca: "Ford", modelo: "Mustang", año: 2024)
]

// CREAR UN ARRAY DE CASAS.
let casas = [
    Casa(dirección: "Calle Mayor", número: "123", habitaciones: 3, baños: 2),
    Casa(dirección: "Calle Menor", número: "234", habitaciones: 4, baños: 3),
    Casa(dirección: "Calle Real", número: "345", habitaciones: 5, baños: 4)
]

// RECORRER EL ARRAY DE PERSONAS Y LLAMAR AL MÉTODO "saludar" PARA CADA PERSONA.
for persona in personas {
    persona.saludar()
}

// RECORRER EL ARRAY DE COCHES Y LLAMAR AL MÉTODO "arrancar" PARA CADA COCHE.
for coche in coches {
    coche.arrancar()
}

// RECORRER EL ARRAY DE CASAS Y LLAMAR AL MÉTODO "abrirPuerta" PARA CADA CASA.
for casa in casas {
    casa.abrirPuerta()
}
```

Este código es un ejemplo de cómo crear clases, objetos y arrays en Swift. También muestra cómo recorrer arrays y llamar a métodos de objetos.