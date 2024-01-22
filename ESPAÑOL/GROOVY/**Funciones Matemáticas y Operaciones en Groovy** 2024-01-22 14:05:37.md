```groovy
// Definición de funciones en Groovy

// Función que devuelve el factorial de un número
def factorial(n) {
  // Si n es 0 o 1, devuelve 1
  if (n <= 1) {
    return 1
  }
  // En caso contrario, calcula el factorial recursivamente
  else {
    return n * factorial(n - 1)
  }
}

// Función que imprime una serie de Fibonacci
def fibonacci(n) {
  // Lista vacía para almacenar los números de Fibonacci
  def fib = []

  // Inicializa la lista con los dos primeros números de Fibonacci
  fib << 0
  fib << 1

  // Genera los números de Fibonacci restantes iterativamente
  for (i in 2..<n) {
    // El siguiente número de Fibonacci es la suma de los dos anteriores
    fib << fib[i - 1] + fib[i - 2]
  }

  // Devuelve la lista de números de Fibonacci
  return fib
}

// Función que comprueba si un número es primo
def esPrimo(n) {
  // Si n es 1, no es primo
  if (n == 1) {
    return false
  }

  // Comprueba si n es divisible por algún número entre 2 y la raíz cuadrada de n
  for (i in 2..Math.sqrt(n)) {
    if (n % i == 0) {
      // Si n es divisible por algún número distinto de 1 y n, no es primo
      return false
    }
  }

  // Si n no es divisible por ningún número entre 2 y su raíz cuadrada, es primo
  return true
}

// Función que calcula el máximo común divisor de dos números
def mcd(a, b) {
  // Mientras b no sea 0, sigue calculando el mcd
  while (b != 0) {
    // El mcd es el resto de la división de a entre b
    def temp = b
    b = a % b
    a = temp
  }

  // Cuando b sea 0, a será el mcd
  return a
}

// Función que calcula el mínimo común múltiplo de dos números
def mcm(a, b) {
  // El mcm es el producto de a y b dividido por el mcd
  return (a * b) / mcd(a, b)
}

// Función principal
def main() {
  // Imprime el factorial de 5
  println "El factorial de 5 es: ${factorial(5)}"

  // Imprime la serie de Fibonacci hasta 10 términos
  println "La serie de Fibonacci hasta 10 términos es: ${fibonacci(10)}"

  // Comprueba si 13 es primo
  println "El número 13 es primo: ${esPrimo(13)}"

  // Calcula el mcd de 12 y 18
  println "El mcd de 12 y 18 es: ${mcd(12, 18)}"

  // Calcula el mcm de 10 y 15
  println "El mcm de 10 y 15 es: ${mcm(10, 15)}"
}

// Invoca la función principal
main()
```

Explicación:

1. Funciones matemáticas:

   - `factorial()` calcula el factorial de un número.
   - `fibonacci()` genera una serie de Fibonacci hasta n términos.
   - `esPrimo()` comprueba si un número es primo.

2. Funciones para el cálculo del máximo común divisor (mcd) y el mínimo común múltiplo (mcm).

3. Función `main()` que invoca a las funciones anteriores y muestra los resultados por pantalla.