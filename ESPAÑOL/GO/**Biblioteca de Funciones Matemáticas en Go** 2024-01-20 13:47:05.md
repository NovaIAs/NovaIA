```go
package complejo

// Función para encontrar el máximo común divisor (MCD) de dos números enteros.
func mcd(a, b int) int {
    if b == 0 {
        return a
    }
    return mcd(b, a%b)
}

// Función para encontrar el mínimo común múltiplo (MCM) de dos números enteros.
func mcm(a, b int) int {
    return (a * b) / mcd(a, b)
}

// Función para generar la serie de Fibonacci hasta un cierto número de términos.
func fibonacci(n int) []int {
    if n <= 0 {
        return nil
    }
    fib := make([]int, n)
    fib[0] = 0
    fib[1] = 1
    for i := 2; i < n; i++ {
        fib[i] = fib[i-1] + fib[i-2]
    }
    return fib
}

// Función para encontrar el factorial de un número entero.
func factorial(n int) int {
    if n == 0 {
        return 1
    }
    return n * factorial(n-1)
}

// Función para comprobar si un número entero es primo.
func esPrimo(n int) bool {
    if n <= 1 {
        return false
    }
    for i := 2; i <= int(math.Sqrt(float64(n))); i++ {
        if n%i == 0 {
            return false
        }
    }
    return true
}

// Función para encontrar todos los números primos hasta un cierto número.
func primosHasta(n int) []int {
    primos := make([]int, 0)
    for i := 2; i <= n; i++ {
        if esPrimo(i) {
            primos = append(primos, i)
        }
    }
    return primos
}

// Función para encontrar el máximo común divisor (MCD) de un array de números enteros.
func mcdArray(nums []int) int {
    mcd := nums[0]
    for _, num := range nums {
        mcd = mcd(mcd, num)
    }
    return mcd
}

// Función para encontrar el mínimo común múltiplo (MCM) de un array de números enteros.
func mcmArray(nums []int) int {
    mcm := nums[0]
    for _, num := range nums {
        mcm = mcm(mcm, num)
    }
    return mcm
}

// Función para generar la serie de Fibonacci hasta un cierto número de términos.
func fibonacciArray(n int) []int {
    if n <= 0 {
        return nil
    }
    fib := make([]int, n)
    fib[0] = 0
    fib[1] = 1
    for i := 2; i < n; i++ {
        fib[i] = fib[i-1] + fib[i-2]
    }
    return fib
}

// Función para encontrar el factorial de un número entero.
func factorialArray(nums []int) []int {
    factoriales := make([]int, len(nums))
    for i, num := range nums {
        factoriales[i] = factorial(num)
    }
    return factoriales
}

// Función para comprobar si un número entero es primo.
func esPrimoArray(nums []int) []bool {
    primos := make([]bool, len(nums))
    for i, num := range nums {
        primos[i] = esPrimo(num)
    }
    return primos
}

// Función para encontrar todos los números primos hasta un cierto número.
func primosHastaArray(nums []int) []int {
    primos := make([]int, 0)
    for _, num := range nums {
        if esPrimo(num) {
            primos = append(primos, num)
        }
    }
    return primos
}
```