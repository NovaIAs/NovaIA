```swift
// Función para encontrar el máximo común divisor de dos números
func mcd(_ a: Int, _ b: Int) -> Int {
    if b == 0 {
        return a
    } else {
        return mcd(b, a % b)
    }
}

// Función para encontrar el mínimo común múltiplo de dos números
func mcm(_ a: Int, _ b: Int) -> Int {
    return (a * b) / mcd(a, b)
}

// Función para encontrar todos los factores primos de un número
func factoresPrimos(_ n: Int) -> [Int] {
    var factoresPrimos: [Int] = []
    var divisor = 2
    while n > 1 {
        if n % divisor == 0 {
            factoresPrimos.append(divisor)
            n /= divisor
        } else {
            divisor += 1
        }
    }
    return factoresPrimos
}

// Función para encontrar todos los números primos hasta un número dado
func numerosPrimos(_ n: Int) -> [Int] {
    var numerosPrimos: [Int] = []
    for i in 2...n {
        var esPrimo = true
        for j in 2..<i {
            if i % j == 0 {
                esPrimo = false
                break
            }
        }
        if esPrimo {
            numerosPrimos.append(i)
        }
    }
    return numerosPrimos
}

// Función para encontrar el número primo más cercano a un número dado
func numeroPrimoMasCercano(_ n: Int) -> Int {
    var siguientePrimo = n + 1
    while !esPrimo(siguientePrimo) {
        siguientePrimo += 1
    }
    return siguientePrimo
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

// Función para encontrar el factorial de un número
func factorial(_ n: Int) -> Int {
    if n == 0 {
        return 1
    } else {
        return n * factorial(n - 1)
    }
}

// Función para generar una secuencia de Fibonacci
func fibonacci(_ n: Int) -> [Int] {
    var secuenciaFibonacci: [Int] = [0, 1]
    while secuenciaFibonacci.count < n {
        let proximoNumero = secuenciaFibonacci[secuenciaFibonacci.count - 1] + secuenciaFibonacci[secuenciaFibonacci.count - 2]
        secuenciaFibonacci.append(proximoNumero)
    }
    return secuenciaFibonacci
}

// Función para encontrar la raíz cuadrada de un número
func raizCuadrada(_ n: Double) -> Double {
    var raiz = n / 2
    while abs(raiz * raiz - n) > 0.0001 {
        raiz = (raiz + n / raiz) / 2
    }
    return raiz
}

```

Explicación del código:

* La función `mcd` encuentra el máximo común divisor de dos números utilizando el algoritmo de Euclides.
* La función `mcm` encuentra el mínimo común múltiplo de dos números utilizando el máximo común divisor.
* La función `factoresPrimos` encuentra todos los factores primos de un número dado.
* La función `numerosPrimos` encuentra todos los números primos hasta un número dado.
* La función `numeroPrimoMasCercano` encuentra el número primo más cercano a un número dado.
* La función `esPrimo` comprueba si un número es primo.
* La función `factorial` encuentra el factorial de un número.
* La función `fibonacci` genera una secuencia de Fibonacci.
* La función `raizCuadrada` encuentra la raíz cuadrada de un número.