```groovy
// Definimos una clase llamada "Calculadora"
class Calculadora {
    // Método para sumar dos números
    def sumar(a, b) {
        return a + b
    }

    // Método para restar dos números
    def restar(a, b) {
        return a - b
    }

    // Método para multiplicar dos números
    def multiplicar(a, b) {
        return a * b
    }

    // Método para dividir dos números
    def dividir(a, b) {
        if (b == 0) {
            throw new ArithmeticException("No se puede dividir por cero")
        }
        return a / b
    }
}

// Creamos una instancia de la clase "Calculadora"
def calculadora = new Calculadora()

// Pedimos al usuario que introduzca dos números
println "Introduce el primer número:"
def primerNumero = System.in.newReader().readLine()

println "Introduce el segundo número:"
def segundoNumero = System.in.newReader().readLine()

// Convertimos los números introducidos a enteros
def num1 = Integer.parseInt(primerNumero)
def num2 = Integer.parseInt(segundoNumero)

// Realizamos las operaciones de suma, resta, multiplicación y división
def suma = calculadora.sumar(num1, num2)
def resta = calculadora.restar(num1, num2)
def multiplicacion = calculadora.multiplicar(num1, num2)
def division = calculadora.dividir(num1, num2)

// Mostramos los resultados en la consola
println "La suma de los dos números es: $suma"
println "La resta de los dos números es: $resta"
println "La multiplicación de los dos números es: $multiplicacion"
println "La división de los dos números es: $division"
```

Este código es un ejemplo de programa que realiza operaciones matemáticas básicas (suma, resta, multiplicación y división) entre dos números introducidos por el usuario.

El código está dividido en dos partes:

* La primera parte define una clase llamada "Calculadora" que contiene los métodos para realizar las operaciones matemáticas.
* La segunda parte crea una instancia de la clase "Calculadora", pide al usuario que introduzca dos números, convierte los números introducidos a enteros y realiza las operaciones matemáticas utilizando los métodos de la clase "Calculadora".

El resultado de las operaciones se muestra en la consola.