**Generador de Código Aleatorio en Español**

```swift
// Importación de bibliotecas necesarias
import Darwin
import Foundation

// Función para generar un número aleatorio entre dos valores
func randomNumber(min: Int, max: Int) -> Int {
    // Se genera un número aleatorio entre 0 y RAND_MAX
    let random = Int(arc4random_uniform(UInt32(RAND_MAX)))
    
    // Se escala el número aleatorio para que esté entre min y max
    let scaledRandom = min + (random % (max - min + 1))
    
    // Se devuelve el número aleatorio escalado
    return scaledRandom
}

// Función para generar una lista de números aleatorios
func randomList(length: Int, min: Int, max: Int) -> [Int] {
    // Se crea una lista vacía para almacenar los números aleatorios
    var list: [Int] = []
    
    // Se genera una lista de números aleatorios de la longitud especificada
    for _ in 0..<length {
        list.append(randomNumber(min: min, max: max))
    }
    
    // Se devuelve la lista de números aleatorios
    return list
}

// Función para generar una cadena aleatoria de una longitud especificada
func randomString(length: Int) -> String {
    // Se crea una cadena vacía para almacenar la cadena aleatoria
    var string: String = ""
    
    // Se genera una cadena aleatoria de la longitud especificada
    for _ in 0..<length {
        // Se genera un número aleatorio entre 0 y 25
        let random = Int(arc4random_uniform(26))
        
        // Se convierte el número aleatorio en una letra mayúscula
        let letter = Character(UnicodeScalar(random + 65)!)
        
        // Se añade la letra a la cadena
        string.append(letter)
    }
    
    // Se devuelve la cadena aleatoria
    return string
}

// Función para generar una lista de cadenas aleatorias
func randomStringList(length: Int, stringLength: Int) -> [String] {
    // Se crea una lista vacía para almacenar las cadenas aleatorias
    var list: [String] = []
    
    // Se genera una lista de cadenas aleatorias de la longitud especificada
    for _ in 0..<length {
        list.append(randomString(length: stringLength))
    }
    
    // Se devuelve la lista de cadenas aleatorias
    return list
}

// Función para imprimir una lista de números aleatorios
func printList(list: [Int]) {
    // Se imprime la lista de números aleatorios
    print("Lista de números aleatorios:")
    for number in list {
        print(number)
    }
}

// Función para imprimir una lista de cadenas aleatorias
func printList(list: [String]) {
    // Se imprime la lista de cadenas aleatorias
    print("Lista de cadenas aleatorias:")
    for string in list {
        print(string)
    }
}

// Generación de una lista de 10 números aleatorios entre 1 y 100
let randomNumberList = randomList(length: 10, min: 1, max: 100)

// Impresión de la lista de números aleatorios
printList(list: randomNumberList)

// Generación de una lista de 5 cadenas aleatorias de longitud 10
let randomStringList = randomStringList(length: 5, stringLength: 10)

// Impresión de la lista de cadenas aleatorias
printList(list: randomStringList)
```

**Explicación del Código**

1. **Funciones para Generar Números Aleatorios:**

    * `randomNumber(min: Int, max: Int) -> Int`: Esta función genera un número aleatorio entre dos valores especificados.
    * `randomList(length: Int, min: Int, max: Int) -> [Int]`: Esta función genera una lista de números aleatorios de una longitud especificada.

2. **Funciones para Generar Cadenas Aleatorias:**

    * `randomString(length: Int) -> String`: Esta función genera una cadena aleatoria de una longitud especificada.
    * `randomStringList(length: Int, stringLength: Int) -> [String]`: Esta función genera una lista de cadenas aleatorias de una longitud especificada.

3. **Funciones para Imprimir Listas:**

    * `printList(list: [Int])`: Esta función imprime una lista de números aleatorios.
    * `printList(list: [String])`: Esta función imprime una lista de cadenas aleatorias.

4. **Generación de Listas Aleatorias:**

    * `randomNumberList`: Se genera una lista de 10 números aleatorios entre 1 y 100.
    * `randomStringList`: Se genera una lista de 5 cadenas aleatorias de longitud 10.

5. **Impresión de Listas Aleatorias:**

    Se imprimen las listas de números aleatorios y cadenas aleatorias utilizando las funciones `printList`.