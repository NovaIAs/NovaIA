```swift
// Primera Función
func factorial(_ n: Int) -> Int {
  if n == 0 {
    return 1
  } else {
    return n * factorial(n-1)
  }
}

// Segunda Función
func fibonacci(_ n: Int) -> Int {
  if n <= 1 {
    return n
  } else {
    return fibonacci(n-1) + fibonacci(n-2)
  }
}

// Clase & Estructuras
class Persona {
  var nombre: String
  var edad: Int

  init(nombre: String, edad: Int) {
    self.nombre = nombre
    self.edad = edad
  }

  func presentarse() {
    print("Hola, mi nombre es \(nombre) y tengo \(edad) años.")
  }
}

struct Punto {
  var x: Double
  var y: Double

  func distancia(a otroPunto: Punto) -> Double {
    let dx = x - otroPunto.x
    let dy = y - otroPunto.y
    return sqrt(dx*dx + dy*dy)
  }
}

// Enum
enum Color: String {
  case rojo = "Rojo"
  case verde = "Verde"
  case azul = "Azul"

  func mezclar(con otroColor: Color) -> Color {
    switch (self, otroColor) {
      case (.rojo, .verde): return .amarillo
      case (.verde, .rojo): return .amarillo
      case (.azul, .rojo): return .morado
      case (.rojo, .azul): return .morado
      case (.azul, .verde): return .cian
      case (.verde, .azul): return .cian
      default: return self
    }
  }
}

// Cierre
let suma = { (a: Int, b: Int) -> Int in
  return a + b
}

// Protocolo
protocol Figura {
  var area: Double { get }
  var perimetro: Double { get }
}

struct Circulo: Figura {
  let radio: Double

  var area: Double {
    return Double.pi * radio * radio
  }

  var perimetro: Double {
    return 2 * Double.pi * radio
  }
}

struct Cuadrado: Figura {
  let lado: Double

  var area: Double {
    return lado * lado
  }

  var perimetro: Double {
    return 4 * lado
  }
}

```

**Explicación:**

* La primera función `factorial` calcula el factorial de un número.
* La segunda función `fibonacci` calcula el número de Fibonacci de un índice.
* La clase `Persona` define una clase simple para representar una persona con un nombre y una edad.
* La estructura `Punto` define una estructura simple para representar un punto en un espacio bidimensional.
* La enumeración `Color` define una enumeración de colores con valores de cadena.
* El cierre `suma` define una función que toma dos enteros y devuelve su suma.
* El protocolo `Figura` define un protocolo que define un área y un perímetro.
* Las estructuras `Circulo` y `Cuadrado` implementan el protocolo `Figura` para círculos y cuadrados.