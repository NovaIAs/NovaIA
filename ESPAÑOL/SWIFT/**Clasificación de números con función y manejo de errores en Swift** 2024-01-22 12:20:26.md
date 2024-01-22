**Clasificación de números con una función y un manejador de errores**

```swift
// Función para clasificar números enteros en pares e impares
func clasificarNumeros(numeros: [Int]) -> (pares: [Int], impares: [Int]) {
    // Inicializar variables para almacenar los números pares e impares
    var pares: [Int] = []
    var impares: [Int] = []

    // Recorrer la lista de números
    for numero in numeros {
        // Comprobar si el número es par o impar
        if numero % 2 == 0 {
            // Número par, añadirlo a la lista de números pares
            pares.append(numero)
        } else {
            // Número impar, añadirlo a la lista de números impares
            impares.append(numero)
        }
    }

    // Devolver una tupla con las listas de números pares e impares
    return (pares, impares)
}

// Manejador de errores personalizados
enum ClasificarNumerosError: Error {
    case listaVacia
}

// Función para clasificar números enteros en pares e impares con manejo de errores
func clasificarNumerosConManejoDeErrores(numeros: [Int]) throws -> (pares: [Int], impares: [Int]) {
    // Comprobar si la lista de números está vacía
    guard !numeros.isEmpty else {
        throw ClasificarNumerosError.listaVacia
    }

    // Llamar a la función de clasificación de números
    return clasificarNumeros(numeros: numeros)
}

// Ejemplo de uso
let numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

// Clasificar números sin manejo de errores
let (pares, impares) = clasificarNumeros(numeros: numeros)
print("Números pares:", pares)
print("Números impares:", impares)

// Clasificar números con manejo de errores
do {
    let (pares, impares) = try clasificarNumerosConManejoDeErrores(numeros: [])
    print("Números pares:", pares)
    print("Números impares:", impares)
} catch ClasificarNumerosError.listaVacia {
    print("Error: La lista de números está vacía")
}
```

**Explicación:**

* La función `clasificarNumeros` toma una lista de números enteros como argumento y devuelve una tupla con dos listas: una lista de números pares y una lista de números impares. La función recorre la lista de números y añade cada número a la lista correspondiente, dependiendo de si es par o impar.

* El manejador de errores personalizados `ClasificarNumerosError` se define como una enumeración. Esta enumeración contiene un único caso, `listaVacia`, que se utiliza para indicar que se ha pasado una lista vacía a la función `clasificarNumerosConManejoDeErrores`.

* La función `clasificarNumerosConManejoDeErrores` toma una lista de números enteros como argumento y devuelve una tupla con dos listas: una lista de números pares y una lista de números impares. La función utiliza el manejo de errores para comprobar si la lista de números está vacía. Si la lista está vacía, se produce una excepción `ClasificarNumerosError.listaVacia`. Si la lista no está vacía, la función llama a la función `clasificarNumeros` para clasificar los números.

* El ejemplo de uso muestra cómo se pueden utilizar las dos funciones para clasificar números. En primer lugar, se clasifican los números sin manejo de errores. A continuación, se intenta clasificar los números con manejo de errores, pero se pasa una lista vacía, lo que provoca una excepción.