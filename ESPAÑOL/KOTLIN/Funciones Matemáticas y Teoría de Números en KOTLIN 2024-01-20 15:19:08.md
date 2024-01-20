```kotlin
// Definir una función para encontrar el factorial de un número
fun factorial(n: Int): Int {
  if (n == 0) {
    return 1
  } else {
    return n * factorial(n - 1)
  }
}

// Definir una función para generar una secuencia de Fibonacci
fun fibonacci(n: Int): List<Int> {
  if (n <= 0) {
    return emptyList()
  } else if (n == 1) {
    return listOf(0)
  } else if (n == 2) {
    return listOf(0, 1)
  } else {
    val fib = fibonacci(n - 1)
    return fib + listOf(fib[fib.size - 1] + fib[fib.size - 2])
  }
}

// Definir una función para encontrar el mínimo común múltiplo de dos números
fun mcm(a: Int, b: Int): Int {
  val mcd = mcd(a, b)
  return (a * b) / mcd
}

// Definir una función para encontrar el máximo común divisor de dos números
fun mcd(a: Int, b: Int): Int {
  if (b == 0) {
    return a
  } else {
    return mcd(b, a % b)
  }
}

// Definir una función para determinar si un número es primo
fun esPrimo(n: Int): Boolean {
  if (n <= 1) {
    return false
  } else if (n <= 3) {
    return true
  } else if (n % 2 == 0 || n % 3 == 0) {
    return false
  } else {
    var i = 5
    while (i * i <= n) {
      if (n % i == 0 || n % (i + 2) == 0) {
        return false
      }
      i += 6
    }
    return true
  }
}

// Definir una función para generar una lista de números primos hasta un número dado
fun primosHasta(n: Int): List<Int> {
  val primos = mutableListOf<Int>()
  for (i in 2..n) {
    if (esPrimo(i)) {
      primos.add(i)
    }
  }
  return primos
}

// Definir una función para generar una lista de todos los divisores de un número
fun divisoresDe(n: Int): List<Int> {
  val divisores = mutableListOf<Int>()
  for (i in 1..n) {
    if (n % i == 0) {
      divisores.add(i)
    }
  }
  return divisores
}

// Definir una función para generar una lista de todos los factores primos de un número
fun factoresPrimosDe(n: Int): List<Int> {
  val factores = mutableListOf<Int>()
  var i = 2
  while (n > 1) {
    if (n % i == 0) {
      factores.add(i)
      n /= i
    } else {
      i++
    }
  }
  return factores
}

// Probar las funciones con algunos ejemplos
println("Factorial de 5: ${factorial(5)}")
println("Secuencia de Fibonacci hasta 10: ${fibonacci(10)}")
println("MCM de 6 y 8: ${mcm(6, 8)}")
println("MCD de 12 y 18: ${mcd(12, 18)}")
println("¿11 es primo?: ${esPrimo(11)}")
println("¿100 es primo?: ${esPrimo(100)}")
println("Números primos hasta 100: ${primosHasta(100)}")
println("Divisores de 12: ${divisoresDe(12)}")
println("Factores primos de 30: ${factoresPrimosDe(30)}")
```

Este código contiene una variedad de funciones matemáticas y de teoría de números. Incluye funciones para encontrar el factorial, generar la secuencia de Fibonacci, encontrar el mínimo común múltiplo y el máximo común divisor, determinar si un número es primo, generar listas de números primos y divisores, y generar listas de factores primos. También incluye ejemplos de cómo usar estas funciones.