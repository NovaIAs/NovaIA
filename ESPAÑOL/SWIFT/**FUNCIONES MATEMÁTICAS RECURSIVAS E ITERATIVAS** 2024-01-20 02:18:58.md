```swift
// Definición de la función que calculará el máximo común divisor de dos números mediante el algoritmo de Euclides.
func maximoComunDivisor(_ a: Int, _ b: Int) -> Int {
    // Caso base: si b es 0, entonces el máximo común divisor es a.
    guard b != 0 else { return a }
    
    // Caso recursivo: si b no es 0, entonces el máximo común divisor es el máximo común divisor de b y el resto de la división de a entre b.
    return maximoComunDivisor(b, a % b)
}

// Definición de la función que calcula la potencia de un número mediante el algoritmo de exponenciación binaria.
func potencia(_ base: Double, _ exponente: Int) -> Double {
    // Caso base: si el exponente es 0, entonces la potencia es 1.
    guard exponente != 0 else { return 1 }

    // Caso recursivo: si el exponente es par, entonces la potencia es igual a la potencia de la base por sí misma elevada a la mitad del exponente.
    if exponente % 2 == 0 {
        let potenciaIntermedia = potencia(base, exponente / 2)
        return potenciaIntermedia * potenciaIntermedia
    } else {
        // Caso recursivo: si el exponente es impar, entonces la potencia es igual a la base por la potencia de la base por sí misma elevada a la mitad del exponente.
        return base * potencia(base, exponente - 1)
    }
}

// Definición de la función que calcula el valor absoluto de un número.
func valorAbsoluto(_ numero: Int) -> Int {
    // Caso base: si el número es negativo, entonces el valor absoluto es el negativo del número.
    guard numero >= 0 else { return -numero }
    
    // Caso recursivo: si el número es positivo, entonces el valor absoluto es el número.
    return numero
}

// Definición de la función que calcula el factorial de un número mediante el algoritmo iterativo.
func factorial(_ numero: Int) -> Int {
    // Caso base: si el número es 0, entonces el factorial es 1.
    guard numero != 0 else { return 1 }
    
    // Caso recursivo: si el número es positivo, entonces el factorial es igual al número por el factorial del número menos 1.
    return numero * factorial(numero - 1)
}

// Definición de la función que calcula el número de Fibonacci de un índice mediante el algoritmo iterativo.
func fibonacci(_ indice: Int) -> Int {
    // Caso base: si el índice es 0, entonces el número de Fibonacci es 0.
    guard indice != 0 else { return 0 }

    // Caso base: si el índice es 1, entonces el número de Fibonacci es 1.
    guard indice != 1 else { return 1 }

    // Caso recursivo: si el índice es mayor que 1, entonces el número de Fibonacci es igual a la suma de los dos números de Fibonacci anteriores.
    return fibonacci(indice - 1) + fibonacci(indice - 2)
}

// Definición de la función que calcula el máximo común divisor de dos números mediante el algoritmo de Euclides recursivo.
func mcdEuclidesRecursivo(_ a: Int, _ b: Int) -> Int {
    // Caso base: si b es 0, entonces el máximo común divisor es a.
    guard b != 0 else { return a }
    
    // Caso recursivo: si b no es 0, entonces el máximo común divisor es el máximo común divisor de b y el resto de la división de a entre b.
    return mcdEuclidesRecursivo(b, a % b)
}

// Definición de la función que calcula la potencia de un número mediante el algoritmo de exponenciación binaria recursiva.
func potenciaRecursiva(_ base: Double, _ exponente: Int) -> Double {
    // Caso base: si el exponente es 0, entonces la potencia es 1.
    guard exponente != 0 else { return 1 }

    // Caso recursivo: si el exponente es par, entonces la potencia es igual a la potencia de la base por sí misma elevada a la mitad del exponente.
    if exponente % 2 == 0 {
        let potenciaIntermedia = potenciaRecursiva(base, exponente / 2)
        return potenciaIntermedia * potenciaIntermedia
    } else {
        // Caso recursivo: si el exponente es impar, entonces la potencia es igual a la base por la potencia de la base por sí misma elevada a la mitad del exponente.
        return base * potenciaRecursiva(base, exponente - 1)
    }
}

// Definición de la función que calcula el valor absoluto de un número mediante el algoritmo iterativo.
func valorAbsolutoIterativo(_ numero: Int) -> Int {
    // Caso base: si el número es negativo, entonces el valor absoluto es el negativo del número.
    if numero < 0 { return -numero }

    // Caso recursivo: si el número es positivo, entonces el valor absoluto es el número.
    return numero
}

// Definición de la función que calcula el factorial de un número mediante el algoritmo iterativo.
func factorialIterativo(_ numero: Int) -> Int {
    // Caso base: si el número es 0, entonces el factorial es 1.
    guard numero >= 1 else { return 1 }

    // Caso recursivo: si el número es positivo, entonces el factorial es igual al número por el factorial del número menos 1.
    var factorial = 1
    for i in 1...numero {
        factorial *= i
    }

    return factorial
}

// Definición de la función que calcula el número de Fibonacci de un índice mediante el algoritmo iterativo.
func fibonacciIterativo(_ indice: Int) -> Int {
    // Caso base: si el índice es 0, entonces el número de Fibonacci es 0.
    if indice == 0 { return 0 }

    // Caso base: si el índice es 1, entonces el número de Fibonacci es 1.
    if indice == 1 { return 1 }

    // Caso recursivo: si el índice es mayor que 1, entonces el número de Fibonacci es igual a la suma de los dos números de Fibonacci anteriores.
    var fibonacci1 = 0
    var fibonacci2 = 1
    var fibonacciActual = 0
    for _ in 2...indice {
        fibonacciActual = fibonacci1 + fibonacci2
        fibonacci1 = fibonacci2
        fibonacci2 = fibonacciActual
    }

    return fibonacciActual
}
```

**Explicación del código:**

* La función `maximoComunDivisor` calcula el máximo común divisor de dos números mediante el algoritmo de Euclides.
* La función `potencia` calcula la potencia de un número mediante el algoritmo de exponenciación binaria.
* La función `valorAbsoluto` calcula el valor absoluto de un número.
* La función `factorial` calcula el factorial de un número mediante el algoritmo iterativo.
* La función `fibonacci` calcula el número de Fibonacci de un índice mediante el algoritmo iterativo.

Además, el código incluye las versiones recursivas de las funciones `maximoComunDivisor`, `potencia` y `valorAbsoluto`.

El código también incluye las versiones iterativas de las funciones `factorial` y `fibonacci`.