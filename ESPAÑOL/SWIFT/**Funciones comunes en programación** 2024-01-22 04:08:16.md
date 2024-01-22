```swift
// Función para calcular el factorial de un número.
func factorial(_ n: Int) -> Int {
    guard n > 0 else { return 1 } // Caso base
    return n * factorial(n - 1) // Recursión
}

// Función para generar una lista de números primos hasta un límite dado.
func numerosPrimos(hasta n: Int) -> [Int] {
    var primos = [2] // Lista inicial con el primer número primo
    var candidato = 3 // Candidato actual para ser número primo
    while candidato <= n { // Bucle para iterar sobre los posibles números primos
        var esPrimo = true // Bandera para indicar si el candidato es primo
        for primo in primos { // Bucle para comprobar si el candidato es divisible por algún primo conocido
            if candidato % primo == 0 {
                esPrimo = false // Si el candidato es divisible por algún primo conocido, no es primo
                break // Salimos del bucle para comprobar el siguiente candidato
            }
        }
        if esPrimo {
            primos.append(candidato) // Si el candidato es primo, lo añadimos a la lista de primos
        }
        candidato += 2 // Incrementamos el candidato en 2 para saltar los números pares
    }
    return primos // Devolvemos la lista de números primos
}

// Función para invertir el orden de los elementos de un array.
func invertirArray<T>(_ array: [T]) -> [T] {
    var invertido = [T]() // Array vacío para almacenar los elementos invertidos
    for elemento in array.reversed() { // Bucle para recorrer el array en orden inverso
        invertido.append(elemento) // Añadimos el elemento actual al array invertido
    }
    return invertido // Devolvemos el array invertido
}

// Función para ordenar un array de objetos por una propiedad.
func ordenarArray<T: Comparable>(_ array: [T], por propiedad: (T) -> Comparable) -> [T] {
    return array.sorted { propiedad($0) < propiedad($1) } // Ordenamos el array usando la función de comparación
}

// Función para buscar el elemento más grande de un array.
func elementoMaximo<T: Comparable>(_ array: [T]) -> T? {
    guard !array.isEmpty else { return nil } // Caso especial para arrays vacíos
    var maximo = array[0] // Inicializamos el máximo con el primer elemento
    for elemento in array { // Bucle para recorrer el array
        if elemento > maximo { // Comparamos el elemento actual con el máximo actual
            maximo = elemento // Si el elemento actual es mayor, lo asignamos como el nuevo máximo
        }
    }
    return maximo // Devolvemos el máximo
}

// Función para buscar el elemento más pequeño de un array.
func elementoMinimo<T: Comparable>(_ array: [T]) -> T? {
    guard !array.isEmpty else { return nil } // Caso especial para arrays vacíos
    var minimo = array[0] // Inicializamos el mínimo con el primer elemento
    for elemento in array { // Bucle para recorrer el array
        if elemento < minimo { // Comparamos el elemento actual con el mínimo actual
            minimo = elemento // Si el elemento actual es menor, lo asignamos como el nuevo mínimo
        }
    }
    return minimo // Devolvemos el mínimo
}
```

Explicación:

1. **Función factorial**: Calcula el factorial de un número usando recursión.

2. **Función numerosPrimos**: Genera una lista de números primos hasta un límite dado.

3. **Función invertirArray**: Invierte el orden de los elementos de un array.

4. **Función ordenarArray**: Ordena un array de objetos por una propiedad.

5. **Función elementoMaximo**: Busca el elemento más grande de un array.

6. **Función elementoMinimo**: Busca el elemento más pequeño de un array.