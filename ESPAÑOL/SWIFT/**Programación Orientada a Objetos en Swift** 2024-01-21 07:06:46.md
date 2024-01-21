```swift
// Un protocolo define una interfaz para un tipo de datos.
// Los tipos que se ajustan a un protocolo deben implementar todos sus requisitos.

protocol Vehículo {
    // Propiedades
    var marca: String { get }
    var modelo: String { get }
    var año: Int { get }

    // Métodos
    func acelerar()
    func frenar()
    func girar(dirección: Dirección)
}

// Una enumeración define un conjunto de valores relacionados.
// Los valores de una enumeración pueden ser utilizados en lugar de enteros o cadenas.

enum Dirección {
    case izquierda
    case derecha
}

// Una clase es una plantilla para crear objetos.
// Una clase define las propiedades y métodos que los objetos de esa clase tendrán.

class Coche: Vehículo {
    // Propiedades
    let marca: String
    let modelo: String
    let año: Int

    // Constructor
    init(marca: String, modelo: String, año: Int) {
        self.marca = marca
        self.modelo = modelo
        self.año = año
    }

    // Métodos
    func acelerar() {
        print("El \(marca) \(modelo) acelera.")
    }

    func frenar() {
        print("El \(marca) \(modelo) frena.")
    }

    func girar(dirección: Dirección) {
        switch dirección {
        case .izquierda:
            print("El \(marca) \(modelo) gira a la izquierda.")
        case .derecha:
            print("El \(marca) \(modelo) gira a la derecha.")
        }
    }
}

// Una función es un bloque de código que puede ser invocado desde otras partes del programa.
// Una función puede tomar parámetros y devolver un valor.

func imprimirInformación(vehículo: Vehículo) {
    print("Marca:", vehículo.marca)
    print("Modelo:", vehículo.modelo)
    print("Año:", vehículo.año)
}

// Un bucle es una estructura de control que permite ejecutar un bloque de código repetidamente.

for i in 0..<10 {
    print("El valor de i es \(i)")
}

// Un condicional es una estructura de control que permite ejecutar un bloque de código si se cumple una condición.

if 10 > 5 {
    print("10 es mayor que 5")
} else {
    print("10 no es mayor que 5")
}

// Un arreglo es una colección ordenada de elementos.
// Los arreglos pueden contener cualquier tipo de dato.

var arreglo = [1, 2, 3, 4, 5]

// Un diccionario es una colección desordenada de pares clave-valor.
// Los diccionarios pueden contener cualquier tipo de dato como clave y valor.

var diccionario = ["Nombre": "Juan", "Edad": 25]

// Una tupla es una colección ordenada de valores.
// Las tuplas pueden contener cualquier tipo de dato.

var tupla = (1, "Hola", 3.14)

// Un opcional es un tipo de dato que puede ser nulo o no nulo.
// Los opcionales se utilizan para representar valores que pueden estar presentes o no.

var opcional: String? = "Hola"

// Un cierre es un bloque de código que puede ser pasado como parámetro a otra función.
// Los cierres pueden capturar variables del ámbito donde fueron definidos.

let cierre = { (x: Int, y: Int) -> Int in
    return x + y
}

// El punto de entrada al programa es la función main.
// La función main se ejecuta cuando se inicia el programa.

func main() {
    // Crear un objeto de la clase Coche
    let coche = Coche(marca: "Toyota", modelo: "Camry", año: 2020)

    // Imprimir la información del coche
    imprimirInformación(vehículo: coche)

    // Usar el coche para acelerar, frenar y girar
    coche.acelerar()
    coche.frenar()
    coche.girar(dirección: .izquierda)

    // Usar el arreglo, el diccionario, la tupla y el opcional
    print("Arreglo:", arreglo)
    print("Diccionario:", diccionario)
    print("Tupla:", tupla)
    print("Opcional:", opcional)

    // Usar el cierre
    let resultado = cierre(10, 20)
    print("Resultado del cierre:", resultado)
}

// Llamar a la función main para iniciar el programa
main()
```

Este código es un ejemplo de un programa complejo en Swift que cubre una amplia variedad de conceptos. El código crea un protocolo, una enumeración, una clase, una función, un bucle, un condicional, un arreglo, un diccionario, una tupla, un opcional, un cierre y llama a la función main para iniciar el programa. El programa imprime la información de un coche, usa el coche para acelerar, frenar y girar, y usa el arreglo, el diccionario, la tupla y el opcional. El programa también usa el cierre para sumar dos números.