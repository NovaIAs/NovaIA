```swift
// Declaración de funciones

// Función para calcular el factorial de un número
func factorial(_ n: Int) -> Int {
    if n == 0 {
        return 1
    } else {
        return n * factorial(n - 1)
    }
}

// Función para calcular la suma de los primeros n números naturales
func sumaPrimerosNaturales(_ n: Int) -> Int {
    if n == 0 {
        return 0
    } else {
        return n + sumaPrimerosNaturales(n - 1)
    }
}

// Función para calcular la potencia de un número elevado a otro
func potencia(_ base: Double, _ exponente: Int) -> Double {
    if exponente == 0 {
        return 1
    } else if exponente < 0 {
        return 1 / potencia(base, -exponente)
    } else {
        return base * potencia(base, exponente - 1)
    }
}

// Función para calcular el máximo común divisor de dos números
func maximoComunDivisor(_ a: Int, _ b: Int) -> Int {
    if b == 0 {
        return a
    } else {
        return maximoComunDivisor(b, a % b)
    }
}

// Función para calcular el mínimo común múltiplo de dos números
func minimoComunMultiplo(_ a: Int, _ b: Int) -> Int {
    return (a * b) / maximoComunDivisor(a, b)
}

// Función para comprobar si un número es primo
func esPrimo(_ n: Int) -> Bool {
    if n <= 1 {
        return false
    }
    for i in 2..<n {
        if n % i == 0 {
            return false
        }
    }
    return true
}

// Función para generar una lista de números primos hasta un número dado
func listaPrimos(_ n: Int) -> [Int] {
    var listaPrimos: [Int] = []
    for i in 2...n {
        if esPrimo(i) {
            listaPrimos.append(i)
        }
    }
    return listaPrimos
}

// Función para encontrar el elemento máximo de una lista
func maximo<T: Comparable>(_ lista: [T]) -> T {
    guard !lista.isEmpty else {
        fatalError("La lista está vacía")
    }
    var maximo = lista[0]
    for elemento in lista {
        if elemento > maximo {
            maximo = elemento
        }
    }
    return maximo
}

// Función para encontrar el elemento mínimo de una lista
func minimo<T: Comparable>(_ lista: [T]) -> T {
    guard !lista.isEmpty else {
        fatalError("La lista está vacía")
    }
    var minimo = lista[0]
    for elemento in lista {
        if elemento < minimo {
            minimo = elemento
        }
    }
    return minimo
}

// Función para ordenar una lista en orden ascendente
func ordenarAscendente<T: Comparable>(_ lista: [T]) -> [T] {
    return lista.sorted()
}

// Función para ordenar una lista en orden descendente
func ordenarDescendente<T: Comparable>(_ lista: [T]) -> [T] {
    return lista.sorted(by: >)
}

// Función para invertir una lista
func invertirLista<T>(_ lista: [T]) -> [T] {
    var listaInvertida: [T] = []
    for elemento in lista.reversed() {
        listaInvertida.append(elemento)
    }
    return listaInvertida
}

// Función para buscar un elemento en una lista
func buscarElemento<T: Equatable>(_ elemento: T, _ lista: [T]) -> Int? {
    for (index, value) in lista.enumerated() {
        if value == elemento {
            return index
        }
    }
    return nil
}

// Función para eliminar un elemento de una lista
func eliminarElemento<T: Equatable>(_ elemento: T, _ lista: [T]) -> [T] {
    var listaFiltrada: [T] = []
    for value in lista {
        if value != elemento {
            listaFiltrada.append(value)
        }
    }
    return listaFiltrada
}

// Función para añadir un elemento a una lista
func añadirElemento<T>(_ elemento: T, _ lista: [T]) -> [T] {
    var listaActualizada: [T] = lista
    listaActualizada.append(elemento)
    return listaActualizada
}

// Función para unir dos listas
func unirListas<T>(_ lista1: [T], _ lista2: [T]) -> [T] {
    var listaUnida: [T] = lista1
    for elemento in lista2 {
        listaUnida.append(elemento)
    }
    return listaUnida
}

// Función para encontrar la intersección de dos listas
func intersecciónListas<T: Equatable>(_ lista1: [T], _ lista2: [T]) -> [T] {
    var intersección: [T] = []
    for elemento in lista1 {
        if lista2.contains(elemento) {
            intersección.append(elemento)
        }
    }
    return intersección
}

// Función para encontrar la diferencia de dos listas
func diferenciaListas<T: Equatable>(_ lista1: [T], _ lista2: [T]) -> [T] {
    var diferencia: [T] = []
    for elemento in lista1 {
        if !lista2.contains(elemento) {
            diferencia.append(elemento)
        }
    }
    return diferencia
}

// Función para contar el número de ocurrencias de un elemento en una lista
func contarOcurrencias<T: Equatable>(_ elemento: T, _ lista: [T]) -> Int {
    var ocurrencias = 0
    for value in lista {
        if value == elemento {
            ocurrencias += 1
        }
    }
    return ocurrencias
}

// Función para filtrar una lista según una condición
func filtrarLista<T>(_ lista: [T], _ condición: (T) -> Bool) -> [T] {
    var listaFiltrada: [T] = []
    for elemento in lista {
        if condición(elemento) {
            listaFiltrada.append(elemento)
        }
    }
    return listaFiltrada
}

// Función para aplicar una transformación a cada elemento de una lista
func transformarLista<T, U>(_ lista: [T], _ transformación: (T) -> U) -> [U] {
    var listaTransformada: [U] = []
    for elemento in lista {
        listaTransformada.append(transformación(elemento))
    }
    return listaTransformada
}

// Función para reducir una lista a un único valor
func reducirLista<T, U>(_ lista: [T], _ reducción: (U, T) -> U) -> U? {
    guard !lista.isEmpty else {
        return nil
    }
    var valorAcumulado: U = lista[0]
    for elemento in lista[1...] {
        valorAcumulado = reducción(valorAcumulado, elemento)
    }
    return valorAcumulado
}

// Función para generar una secuencia de números desde un número inicial hasta un número final con un paso especificado
func generarSecuencia(_ inicio: Int, _ fin: Int, _ paso: Int = 1) -> [Int] {
    var secuencia: [Int] = []
    var númeroActual = inicio
    while númeroActual <= fin {
        secuencia.append(númeroActual)
        númeroActual += paso
    }
    return secuencia
}

// Función para generar una secuencia de números desde un número inicial hasta un número final en orden descendente con un paso especificado
func generarSecuenciaDescendente(_ inicio: Int, _ fin: Int, _ paso: Int = 1) -> [Int] {
    var secuencia: [Int] = []
    var númeroActual