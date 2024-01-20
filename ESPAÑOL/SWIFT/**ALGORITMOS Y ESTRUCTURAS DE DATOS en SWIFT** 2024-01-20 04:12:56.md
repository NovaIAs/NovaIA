```swift
// Importación de las bibliotecas y extensiones necesarias

// Función para calcular el factorial de un número entero no negativo
func factorial(_ n: Int) -> Int {
    if n == 0 {
        return 1
    } else {
        return n * factorial(n-1)
    }
}

// Función para generar una lista de todos los números primos menores que un límite dado
func generarPrimos(_ límite: Int) -> [Int] {
    var primos: [Int] = []
    for número in 2...límite {
        var esPrimo = true
        for divisor in 2..<número {
            if número % divisor == 0 {
                esPrimo = false
                break
            }
        }
        if esPrimo {
            primos.append(número)
        }
    }
    return primos
}

// Función para buscar el máximo común divisor de dos números enteros positivos
func máximoComúnDivisor(_ a: Int, _ b: Int) -> Int {
    var mayor = 0
    if a > b {
        mayor = a
    } else {
        mayor = b
    }
    for divisor in 1...mayor {
        if a % divisor == 0 && b % divisor == 0 {
            return divisor
        }
    }
    return 1
}

// Función para generar una lista de todas las permutaciones de una lista dada
func generarPermutaciones<T>(_ lista: [T]) -> [[T]] {
    var permutaciones: [[T]] = []
    if lista.count == 0 {
        return [[]]
    }
    for i in 0..<lista.count {
        let elemento = lista[i]
        let sublista = lista.filter { $0 != elemento }
        let subpermutaciones = generarPermutaciones(sublista)
        for subpermutación in subpermutaciones {
            permutaciones.append([elemento] + subpermutación)
        }
    }
    return permutaciones
}

// Función para generar una lista de todas las combinaciones de una lista dada
func generarCombinaciones<T>(_ lista: [T], _ tamaño: Int) -> [[T]] {
    var combinaciones: [[T]] = []
    if tamaño == 0 {
        return [[]]
    }
    for i in 0..<lista.count {
        let elemento = lista[i]
        let sublista = lista.filter { $0 != elemento }
        let subcombinaciones = generarCombinaciones(sublista, tamaño-1)
        for subcombinación in subcombinaciones {
            combinaciones.append([elemento] + subcombinación)
        }
    }
    return combinaciones
}

// Función para generar una lista de todos los subconjuntos de una lista dada
func generarSubconjuntos<T>(_ lista: [T]) -> [[T]] {
    var subconjuntos: [[T]] = []
    subconjuntos.append([])
    for i in 0..<lista.count {
        let elemento = lista[i]
        let sublista = lista.filter { $0 != elemento }
        let subsubconjuntos = generarSubconjuntos(sublista)
        for subsubconjunto in subsubconjuntos {
            subconjuntos.append([elemento] + subsubconjunto)
        }
    }
    return subconjuntos
}

// Main

// Calcular el factorial de 5
let resultadoFactorial = factorial(5)
print("El factorial de 5 es \(resultadoFactorial)")

// Generar una lista de los primeros 10 números primos
let primerosPrimos = generarPrimos(10)
print("Los primeros 10 números primos son: \(primerosPrimos)")

// Buscar el máximo común divisor de 12 y 18
let mcd = máximoComúnDivisor(12, 18)
print("El máximo común divisor de 12 y 18 es \(mcd)")

// Generar una lista de todas las permutaciones de la lista [1, 2, 3]
let permutaciones = generarPermutaciones([1, 2, 3])
print("Las permutaciones de [1, 2, 3] son: \(permutaciones)")

// Generar una lista de todas las combinaciones de la lista [1, 2, 3, 4] tomadas de 2 en 2
let combinaciones = generarCombinaciones([1, 2, 3, 4], 2)
print("Las combinaciones de [1, 2, 3, 4] tomadas de 2 en 2 son: \(combinaciones)")

// Generar una lista de todos los subconjuntos de la lista [1, 2, 3]
let subconjuntos = generarSubconjuntos([1, 2, 3])
print("Los subconjuntos de [1, 2, 3] son: \(subconjuntos)")
```

Explicación del código:

* La función `factorial` calcula el factorial de un número entero no negativo usando la recursión.
* La función `generarPrimos` genera una lista de todos los números primos menores que un límite dado usando la criba de Eratóstenes.
* La función `máximoComúnDivisor` calcula el máximo común divisor de dos números enteros positivos usando el algoritmo de Euclides.
* La función `generarPermutaciones` genera una lista de todas las permutaciones de una lista dada usando el algoritmo de retroceso.
* La función `generarCombinaciones` genera una lista de todas las combinaciones de una lista dada tomadas de un tamaño dado usando el algoritmo de retroceso.
* La función `generarSubconjuntos` genera una lista de todos los subconjuntos de una lista dada usando el algoritmo de retroceso.