```scala
// Definimos un objeto llamado "HolaMundo"
object HolaMundo {

    // Definimos una función llamada "saludar" que recibe un nombre como parámetro
    def saludar(nombre: String): Unit = {
        // Imprimimos un mensaje de saludo usando el nombre recibido como parámetro
        println(s"¡Hola, $nombre! ¿Cómo estás?")
    }

    // Definimos una función llamada "sumar" que recibe dos números como parámetros
    def sumar(num1: Int, num2: Int): Int = {
        // Sumamos los dos números y devolvemos el resultado
        num1 + num2
    }

    // Definimos una función llamada "calcularAreaCuadrado" que recibe la longitud de un lado del cuadrado como parámetro
    def calcularAreaCuadrado(lado: Double): Double = {
        // Calculamos el área del cuadrado y devolvemos el resultado
        lado * lado
    }

    // Definimos una función llamada "promedio" que recibe una lista de números como parámetro
    def promedio(numeros: List[Int]): Double = {
        // Sumamos todos los números de la lista
        val suma = numeros.sum

        // Dividimos la suma entre el número de elementos de la lista para obtener el promedio
        suma / numeros.length
    }

    // Definimos una clase llamada "Persona" que tiene dos propiedades: nombre y edad
    class Persona(val nombre: String, val edad: Int) {

        // Definimos un método llamado "saludar" que imprime un mensaje de saludo
        def saludar(): Unit = {
            println(s"Hola, mi nombre es $nombre y tengo $edad años.")
        }
    }

    // Definimos una función principal
    def main(args: Array[String]): Unit = {

        // Llamamos a la función "saludar" para saludar al usuario
        saludar("Juan")

        // Llamamos a la función "sumar" para sumar dos números
        val resultadoSuma = sumar(5, 10)
        println(s"La suma de 5 y 10 es: $resultadoSuma")

        // Llamamos a la función "calcularAreaCuadrado" para calcular el área de un cuadrado
        val resultadoAreaCuadrado = calcularAreaCuadrado(5.0)
        println(s"El área de un cuadrado de 5 unidades de lado es: $resultadoAreaCuadrado")

        // Llamamos a la función "promedio" para calcular el promedio de una lista de números
        val numeros = List(1, 2, 3, 4, 5)
        val resultadoPromedio = promedio(numeros)
        println(s"El promedio de los números 1, 2, 3, 4 y 5 es: $resultadoPromedio")

        // Creamos un objeto de la clase "Persona"
        val persona = new Persona("María", 30)

        // Llamamos al método "saludar" del objeto "persona" para saludar al usuario
        persona.saludar()
    }
}
```

Explicación del código:

* El código define un objeto llamado "HolaMundo" que contiene varias funciones y una clase.
* La función "saludar" recibe un nombre como parámetro e imprime un mensaje de saludo usando el nombre recibido.
* La función "sumar" recibe dos números como parámetros y devuelve la suma de los dos números.
* La función "calcularAreaCuadrado" recibe la longitud de un lado del cuadrado como parámetro y devuelve el área del cuadrado.
* La función "promedio" recibe una lista de números como parámetro y devuelve el promedio de los números de la lista.
* La clase "Persona" tiene dos propiedades: "nombre" y "edad". También define un método llamado "saludar" que imprime un mensaje de saludo.
* La función principal "main" llama a las diferentes funciones y al método "saludar" del objeto "persona" para mostrar los resultados por pantalla.