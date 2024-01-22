```scala
// Definición de un objeto
object Calculadora {

  // Función para sumar dos números
  def sumar(a: Int, b: Int): Int = {
    a + b
  }

  // Función para restar dos números
  def restar(a: Int, b: Int): Int = {
    a - b
  }

  // Función para multiplicar dos números
  def multiplicar(a: Int, b: Int): Int = {
    a * b
  }

  // Función para dividir dos números
  def dividir(a: Int, b: Int): Int = {
    a / b
  }

  // Función principal
  def main(args: Array[String]): Unit = {

    // Se solicita el primer número al usuario
    println("Introduce el primer número:")

    // Se lee el primer número de la entrada estándar
    val primerNumero: Int = readLine.toInt

    // Se solicita el segundo número al usuario
    println("Introduce el segundo número:")

    // Se lee el segundo número de la entrada estándar
    val segundoNumero: Int = readLine.toInt

    // Se imprime el resultado de la suma de los dos números
    println(s"La suma de los dos números es: ${sumar(primerNumero, segundoNumero)}")

    // Se imprime el resultado de la resta de los dos números
    println(s"La resta de los dos números es: ${restar(primerNumero, segundoNumero)}")

    // Se imprime el resultado de la multiplicación de los dos números
    println(s"La multiplicación de los dos números es: ${multiplicar(primerNumero, segundoNumero)}")

    // Se imprime el resultado de la división de los dos números
    println(s"La división de los dos números es: ${dividir(primerNumero, segundoNumero)}")
  }
}
```

**Explicación del código:**

* El objeto `Calculadora` contiene cuatro funciones para realizar operaciones aritméticas básicas: `sumar`, `restar`, `multiplicar` y `dividir`.
* La función `main` es el punto de entrada del programa.
* En la función `main`, se solicitan al usuario los dos números que se van a operar.
* Los números introducidos por el usuario se convierten a enteros mediante la función `toInt`.
* Se llaman a las funciones `sumar`, `restar`, `multiplicar` y `dividir` para realizar las operaciones aritméticas básicas.
* Los resultados de las operaciones se imprimen en la salida estándar mediante la función `println`.