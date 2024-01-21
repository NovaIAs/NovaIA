```swift
// Incluir bibliotecas necesarias
import Foundation
import UIKit

// Definir una clase personalizada
class MiClasePersonalizada {

    // Propiedades
    var nombre: String
    var edad: Int

    // Constructor
    init(nombre: String, edad: Int) {
        self.nombre = nombre
        self.edad = edad
    }

    // Método para saludar
    func saludar() {
        print("Hola, mi nombre es \(nombre) y tengo \(edad) años.")
    }
}

// Crear una instancia de la clase personalizada
let persona = MiClasePersonalizada(nombre: "Juan", edad: 25)

// Llamar al método saludar
persona.saludar()

// Definir una función para calcular el factorial de un número
func factorial(_ numero: Int) -> Int {
    if numero == 0 {
        return 1
    } else {
        return numero * factorial(numero - 1)
    }
}

// Calcular el factorial de 5
let factorial5 = factorial(5)

// Imprimir el resultado
print("El factorial de 5 es \(factorial5)")

// Definir un protocolo para figuras geométricas
protocol FiguraGeometrica {

    // Propiedades
    var ancho: Double { get }
    var altura: Double { get }

    // Métodos
    func area() -> Double
    func perimetro() -> Double
}

// Definir una clase para un rectángulo
class Rectangulo: FiguraGeometrica {

    // Propiedades
    var ancho: Double
    var altura: Double

    // Constructor
    init(ancho: Double, altura: Double) {
        self.ancho = ancho
        self.altura = altura
    }

    // Métodos
    func area() -> Double {
        return ancho * altura
    }

    func perimetro() -> Double {
        return 2 * (ancho + altura)
    }
}

// Definir una clase para un círculo
class Circulo: FiguraGeometrica {

    // Propiedades
    var radio: Double

    // Constructor
    init(radio: Double) {
        self.radio = radio
    }

    // Métodos
    var ancho: Double {
        return 2 * radio
    }

    var altura: Double {
        return 2 * radio
    }

    func area() -> Double {
        return Double.pi * radio * radio
    }

    func perimetro() -> Double {
        return 2 * Double.pi * radio
    }
}

// Crear una instancia de un rectángulo y un círculo
let rectangulo = Rectangulo(ancho: 5.0, altura: 10.0)
let circulo = Circulo(radio: 5.0)

// Calcular el área y el perímetro de cada figura
let areaRectangulo = rectangulo.area()
let perimetroRectangulo = rectangulo.perimetro()

let areaCirculo = circulo.area()
let perimetroCirculo = circulo.perimetro()

// Imprimir los resultados
print("Área del rectángulo: \(areaRectangulo)")
print("Perímetro del rectángulo: \(perimetroRectangulo)")

print("Área del círculo: \(areaCirculo)")
print("Perímetro del círculo: \(perimetroCirculo)")

// Definir una estructura para un punto en el espacio
struct Punto {

    // Propiedades
    var x: Double
    var y: Double
    var z: Double

    // Constructor
    init(x: Double, y: Double, z: Double) {
        self.x = x
        self.y = y
        self.z = z
    }

    // Métodos
    func distancia(a punto: Punto) -> Double {
        let dx = punto.x - x
        let dy = punto.y - y
        let dz = punto.z - z

        return sqrt(dx * dx + dy * dy + dz * dz)
    }
}

// Crear dos puntos en el espacio
let punto1 = Punto(x: 1.0, y: 2.0, z: 3.0)
let punto2 = Punto(x: 4.0, y: 5.0, z: 6.0)

// Calcular la distancia entre los dos puntos
let distancia = punto1.distancia(a: punto2)

// Imprimir el resultado
print("Distancia entre los dos puntos: \(