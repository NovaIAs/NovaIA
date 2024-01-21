```kotlin
//Creamos una clase llamada Calculadora que tendrá métodos para realizar operaciones matemáticas.

class Calculadora {
    //Creamos un método para sumar dos números.

    fun sumar(numero1: Double, numero2: Double): Double {
        return numero1 + numero2
    }

    //Creamos un método para restar dos números.

    fun restar(numero1: Double, numero2: Double): Double {
        return numero1 - numero2
    }

    //Creamos un método para multiplicar dos números.

    fun multiplicar(numero1: Double, numero2: Double): Double {
        return numero1 * numero2
    }

    //Creamos un método para dividir dos números.

    fun dividir(numero1: Double, numero2: Double): Double {
        if (numero2 == 0.0) {
            throw ArithmeticException("No se puede dividir por cero")
        }
        return numero1 / numero2
    }

    //Creamos un método para elevar un número a una potencia.

    fun potencia(numero1: Double, numero2: Double): Double {
        return Math.pow(numero1, numero2)
    }

    //Creamos un método para calcular la raíz cuadrada de un número.

    fun raizCuadrada(numero1: Double): Double {
        return Math.sqrt(numero1)
    }
}

//Creamos un objeto de la clase Calculadora para poder usar sus métodos.

val calculadora = Calculadora()

// Usamos el método sumar para sumar dos números.

val resultadoSuma = calculadora.sumar(2.0, 3.0)

// Usamos el método restar para restar dos números.

val resultadoResta = calculadora.restar(5.0, 2.0)

// Usamos el método multiplicar para multiplicar dos números.

val resultadoMultiplicacion = calculadora.multiplicar(3.0, 4.0)

// Usamos el método dividir para dividir dos números.

val resultadoDivision = calculadora.dividir(10.0, 2.0)

// Usamos el método potencia para elevar un número a una potencia.

val resultadoPotencia = calculadora.potencia(2.0, 3.0)

// Usamos el método raizCuadrada para calcular la raíz cuadrada de un número.

val resultadoRaizCuadrada = calculadora.raizCuadrada(9.0)

//Imprimimos los resultados en la consola.

println("Resultado de la suma: $resultadoSuma")
println("Resultado de la resta: $resultadoResta")
println("Resultado de la multiplicación: $resultadoMultiplicacion")
println("Resultado de la división: $resultadoDivision")
println("Resultado de la potencia: $resultadoPotencia")
println("Resultado de la raíz cuadrada: $resultadoRaizCuadrada")
```

Explicación del código:

1. Creamos una clase llamada `Calculadora` que tiene varios métodos para realizar operaciones matemáticas como sumar, restar, multiplicar, dividir, elevar a una potencia y calcular la raíz cuadrada.

2. Creamos un objeto de la clase `Calculadora` llamado `calculadora` para poder usar sus métodos.

3. Usamos los métodos de la clase `Calculadora` para realizar diferentes operaciones matemáticas con números y almacenamos los resultados en variables.

4. Imprimimos los resultados de las operaciones matemáticas en la consola.