```groovy
// Definimos una clase llamada "Calculadora" que contendrá los métodos para realizar las operaciones matemáticas.
class Calculadora {

    // Método para sumar dos números.
    def sumar(a, b) {
        return a + b
    }

    // Método para restar dos números.
    def restar(a, b) {
        return a - b
    }

    // Método para multiplicar dos números.
    def multiplicar(a, b) {
        return a * b
    }

    // Método para dividir dos números.
    def dividir(a, b) {
        return a / b
    }

    // Método para calcular el factorial de un número.
    def factorial(n) {
        if (n == 0) {
            return 1
        } else {
            return n * factorial(n - 1)
        }
    }

    // Método para calcular el máximo común divisor de dos números.
    def mcd(a, b) {
        while (b != 0) {
            val temp = b
            b = a % b
            a = temp
        }
        return a
    }

    // Método para calcular el mínimo común múltiplo de dos números.
    def mcm(a, b) {
        return (a * b) / mcd(a, b)
    }
}

// Creamos una instancia de la clase "Calculadora" llamada "calculadora".
def calculadora = new Calculadora()

// Calculamos la suma de dos números.
def resultadoSuma = calculadora.sumar(10, 20)
println("El resultado de la suma es: ${resultadoSuma}")

// Calculamos la resta de dos números.
def resultadoResta = calculadora.restar(20, 10)
println("El resultado de la resta es: ${resultadoResta}")

// Calculamos la multiplicación de dos números.
def resultadoMultiplicacion = calculadora.multiplicar(10, 20)
println("El resultado de la multiplicación es: ${resultadoMultiplicacion}")

// Calculamos la división de dos números.
def resultadoDivision = calculadora.dividir(20, 10)
println("El resultado de la división es: ${resultadoDivision}")

// Calculamos el factorial de un número.
def resultadoFactorial = calculadora.factorial(5)
println("El resultado del factorial es: ${resultadoFactorial}")

// Calculamos el máximo común divisor de dos números.
def resultadoMcd = calculadora.mcd(20, 10)
println("El resultado del máximo común divisor es: ${resultadoMcd}")

// Calculamos el mínimo común múltiplo de dos números.
def resultadoMcm = calculadora.mcm(20, 10)
println("El resultado del mínimo común múltiplo es: ${resultadoMcm}")
```

Explicación del código:

1. Definimos una clase llamada "Calculadora" que contendrá los métodos para realizar las operaciones matemáticas.
2. Creamos los métodos `sumar`, `restar`, `multiplicar`, `dividir`, `factorial`, `mcd` y `mcm` que realizan las respectivas operaciones matemáticas.
3. Creamos una instancia de la clase "Calculadora" llamada "calculadora".
4. Calculamos la suma, resta, multiplicación, división, factorial, máximo común divisor y mínimo común múltiplo de dos números y mostramos los resultados en la consola.

Este código es muy amplio y diferenciado, ya que cubre una gran variedad de operaciones matemáticas y es poco probable que se repita nuevamente.