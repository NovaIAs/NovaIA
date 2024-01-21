```swift
// Declaración de constantes y variables

let nombre = "Juan"
var edad = 25

// Estructuras

struct Persona {
    let nombre: String
    var edad: Int
}

let persona1 = Persona(nombre: "María", edad: 30)

// Clases

class Animal {
    let nombre: String

    init(nombre: String) {
        self.nombre = nombre
    }

    func hablar() {
        print("...")
    }
}

class Perro: Animal {
    override func hablar() {
        print("Woof!")
    }
}

let perro1 = Perro(nombre: "Toby")

// Funciones

func suma(a: Int, b: Int) -> Int {
    return a + b
}

let resultado = suma(a: 1, b: 2)

// Closures

let cuadrado = { (x: Int) -> Int in
    return x * x
}

let resultadoCuadrado = cuadrado(5)

// Operadores

let x = 10
let y = 5

let suma = x + y
let resta = x - y
let multiplicacion = x * y
let division = x / y
let modulo = x % y

// Condicionales

if resultado > 10 {
    print("El resultado es mayor que 10")
} else if resultado == 10 {
    print("El resultado es igual a 10")
} else {
    print("El resultado es menor que 10")
}

// Bucles

for i in 1...10 {
    print(i)
}

// Rangos

let rango = 1...10

for i in rango {
    print(i)
}

// Diccionarios

var diccionario: [String: Int] = ["Juan": 25, "María": 30]

diccionario["Juan"] // 25

// Conjuntos

var conjunto: Set<Int> = [1, 2, 3, 4, 5]

conjunto.insert(6) // Añade 6 al conjunto

conjunto.remove(3) // Elimina 3 del conjunto

// Tuplas

let tupla = (1, "Juan")

tupla.0 // 1
tupla.1 // "Juan"

// Extensiones

extension Array where Element: Equatable {
    func removeDuplicates() -> [Element] {
        var result: [Element] = []
        for element in self {
            if !result.contains(element) {
                result.append(element)
            }
        }
        return result
    }
}

let array = [1, 2, 3, 4, 5, 1, 2, 3]

let arraySinDuplicados = array.removeDuplicates()

```

Este código es un ejemplo de un código complejo en Swift. Incluye una variedad de características del lenguaje, como constantes, variables, estructuras, clases, funciones, cierres, operadores, condicionales, bucles, rangos, diccionarios, conjuntos, tuplas y extensiones. El código también está bien documentado con comentarios en español.