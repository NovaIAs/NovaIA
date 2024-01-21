```swift
// Definición de clase
class Calculadora {

    // Método para sumar dos números
    func sumar(a: Int, b: Int) -> Int {
        return a + b
    }

    // Método para restar dos números
    func restar(a: Int, b: Int) -> Int {
        return a - b
    }

    // Método para multiplicar dos números
    func multiplicar(a: Int, b: Int) -> Int {
        return a * b
    }

    // Método para dividir dos números
    func dividir(a: Int, b: Int) -> Double {
        // Comprobación para evitar divisiones por cero
        guard b != 0 else {
            return Double.infinity
        }

        return Double(a) / Double(b)
    }

    // Método para calcular el factorial de un número
    func factorial(n: Int) -> Int {
        // Condición inicial: factorial de 0 es 1
        if n == 0 {
            return 1
        }

        // Recursión: calcular el factorial de n como n * factorial(n-1)
        return n * factorial(n: n-1)
    }

    // Método para calcular el máximo común divisor de dos números
    func maximoComunDivisor(a: Int, b: Int) -> Int {
        // Condición inicial: si b es 0, el mcd es a
        if b == 0 {
            return a
        }

        // Recursividad: calcular el mcd de a y b como el mcd de b y el resto de a dividido por b
        return maximoComunDivisor(a: b, b: a % b)
    }

    // Método para calcular el mínimo común múltiplo de dos números
    func minimoComunMultiplo(a: Int, b: Int) -> Int {
        return a * b / maximoComunDivisor(a: a, b: b)
    }

    // Método para calcular la potencia de un número elevado a otro
    func potencia(base: Double, exponente: Int) -> Double {
        // Condición inicial: base elevado a 0 es 1
        if exponente == 0 {
            return 1
        }

        // Recursión: calcular la potencia de base elevado a exponente como base * potencia(base, exponente-1)
        return base * potencia(base: base, exponente: exponente-1)
    }

    // Método para calcular la raíz cuadrada de un número
    func raizCuadrada(n: Double) -> Double {
        // Condición inicial: raíz cuadrada de 0 es 0
        if n == 0 {
            return 0
        }

        // Usar la fórmula de Newton-Raphson para obtener una aproximación de la raíz cuadrada
        var x = n / 2.0
        while abs(x * x - n) > 0.0001 {
            x = (x + n / x) / 2.0
        }

        return x
    }
}

// Crear una instancia de la clase Calculadora
let calculadora = Calculadora()

// Utilizar los métodos de la clase para realizar cálculos
let suma = calculadora.sumar(a: 10, b: 5)
let resta = calculadora.restar(a: 10, b: 5)
let multiplicacion = calculadora.multiplicar(a: 10, b: 5)
let division = calculadora.dividir(a: 10, b: 5)
let factorial = calculadora.factorial(n: 5)
let mcd = calculadora.maximoComunDivisor(a: 12, b: 18)
let mcm = calculadora.minimoComunMultiplo(a: 12, b: 18)
let potencia = calculadora.potencia(base: 2, exponente: 5)
let raizCuadrada = calculadora.raizCuadrada(n: 9)

// Imprimir los resultados de los cálculos
print("Suma: \(suma)")
print("Resta: \(resta)")
print("Multiplicación: \(multiplicacion)")
print("División: \(division)")
print("Factorial: \(factorial)")
print("Máximo común divisor: \(mcd)")
print("Mínimo común múltiplo: \(mcm)")
print("Potencia: \(potencia)")
print("Raíz cuadrada: \(raizCuadrada)")
```

Este código define una clase llamada Calculadora que contiene una serie de métodos matemáticos comunes. Estos métodos incluyen la suma, la resta, la multiplicación, la división, el cálculo del factorial de un número, el cálculo del máximo común divisor y el mínimo común múltiplo de dos números, el cálculo de la potencia de un número elevado a otro y el cálculo de la raíz cuadrada de un número.

La clase Calculadora se utiliza para crear instancias de sí misma que pueden ser utilizadas para realizar cálculos matemáticos. Por ejemplo, el siguiente código crea una instancia de la clase Calculadora llamada calculadora:

```swift
let calculadora = Calculadora()
```

A continuación, el código utiliza los métodos de la clase Calculadora para realizar cálculos matemáticos. Por ejemplo, el siguiente código calcula la suma de 10 y 5:

```swift
let suma = calculadora.sumar(a: 10, b: 5)
```

El resultado del cálculo se almacena en la variable suma.

El código continúa utilizando los métodos de la clase Calculadora para realizar otros cálculos matemáticos.

Finalmente, el código imprime los resultados de los cálculos en la consola.