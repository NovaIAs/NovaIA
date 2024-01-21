```swift
// Clases y Objetos

class Persona {
  var nombre: String
  var edad: Int

  init(nombre: String, edad: Int) {
    self.nombre = nombre
    self.edad = edad
  }

  func saludar() {
    print("Hola, mi nombre es \(nombre) y tengo \(edad) años.")
  }
}

let persona1 = Persona(nombre: "Juan", edad: 25)
let persona2 = Persona(nombre: "María", edad: 30)

persona1.saludar() // Hola, mi nombre es Juan y tengo 25 años.
persona2.saludar() // Hola, mi nombre es María y tengo 30 años.

// Funciones

func suma(num1: Int, num2: Int) -> Int {
  return num1 + num2
}

let resultado = suma(num1: 10, num2: 20) // resultado = 30

// Closures

let numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

let numerosPares = numeros.filter { $0 % 2 == 0 } // [2, 4, 6, 8, 10]

// Protocolos

protocol FiguraGeometrica {
  var area: Double { get }
  var perimetro: Double { get }
}

struct Cuadrado: FiguraGeometrica {
  var lado: Double

  var area: Double {
    return lado * lado
  }

  var perimetro: Double {
    return 4 * lado
  }
}

struct Circulo: FiguraGeometrica {
  var radio: Double

  var area: Double {
    return .pi * radio * radio
  }

  var perimetro: Double {
    return 2 * .pi * radio
  }
}

let cuadrado = Cuadrado(lado: 5.0)
let circulo = Circulo(radio: 3.0)

print("Área del cuadrado: \(cuadrado.area)") // Área del cuadrado: 25.0
print("Perímetro del cuadrado: \(cuadrado.perimetro)") // Perímetro del cuadrado: 20.0
print("Área del círculo: \(circulo.area)") // Área del círculo: 28.274333882308138
print("Perímetro del círculo: \(circulo.perimetro)") // Perímetro del círculo: 18.84955592153876

// Generadores

func generarNumerosPares(hasta: Int) -> AnyIterator<Int> {
  var numeroActual = 0

  return AnyIterator {
    guard numeroActual <= hasta else { return nil }
    defer { numeroActual += 2 }
    return numeroActual
  }
}

for numero in generarNumerosPares(hasta: 10) {
  print(numero) // 0, 2, 4, 6, 8, 10
}
```