```kotlin
// Clase principal
class Main {

    // Función principal
    fun main(args: Array<String>) {

        // Crear una lista de números enteros
        val numeros = listOf(1, 2, 3, 4, 5)

        // Filtrar los números pares de la lista
        val pares = numeros.filter { it % 2 == 0 }

        // Imprimir los números pares en la consola
        println("Números pares:")
        for (numero in pares) {
            println(numero)
        }

        // Crear un mapa de cadenas de texto a números enteros
        val mapa = mapOf("uno" to 1, "dos" to 2, "tres" to 3)

        // Obtener el valor asociado a una clave del mapa
        val valor = mapa["dos"]

        // Imprimir el valor obtenido en la consola
        println("Valor obtenido:")
        println(valor)

        // Crear una función que reciba dos números enteros y devuelva su suma
        fun sumar(a: Int, b: Int): Int {
            return a + b
        }

        // Llamar a la función sumar y almacenar el resultado en una variable
        val resultado = sumar(3, 5)

        // Imprimir el resultado en la consola
        println("Resultado:")
        println(resultado)

        // Crear una clase de persona
        class Persona(val nombre: String, val edad: Int)

        // Crear una instancia de la clase persona
        val persona = Persona("Juan", 20)

        // Imprimir el nombre de la persona en la consola
        println("Nombre:")
        println(persona.nombre)

        // Imprimir la edad de la persona en la consola
        println("Edad:")
        println(persona.edad)
    }
}
```

Explicación:

1. **Clase principal:** La clase `Main` es la clase principal del código. Contiene la función `main`, que es el punto de entrada del programa.

2. **Función principal:** La función `main` se ejecuta cuando se inicia el programa. Crea una lista de números enteros, filtra los números pares de la lista, imprime los números pares en la consola, crea un mapa de cadenas de texto a números enteros, obtiene el valor asociado a una clave del mapa, imprime el valor obtenido en la consola, crea una función que recibe dos números enteros y devuelve su suma, llama a la función sumar y almacena el resultado en una variable, imprime el resultado en la consola, crea una clase de persona, crea una instancia de la clase persona e imprime el nombre y la edad de la persona en la consola.

3. **Lista de números enteros:** Una lista es una colección de elementos que se pueden acceder por su índice. En este caso, la lista contiene cinco números enteros: 1, 2, 3, 4 y 5.

4. **Filtrar los números pares de la lista:** La función `filter` se utiliza para filtrar los elementos de una colección que cumplen una condición. En este caso, la condición es que el número sea par. La función devuelve una nueva colección con los elementos que cumplen la condición.

5. **Imprimir los números pares en la consola:** La función `println` se utiliza para imprimir un mensaje en la consola. En este caso, se imprime un mensaje con los números pares de la lista.

6. **Mapa de cadenas de texto a números enteros:** Un mapa es una colección de pares clave-valor. En este caso, el mapa contiene tres pares clave-valor: "uno" y 1, "dos" y 2, y "tres" y 3.

7. **Obtener el valor asociado a una clave del mapa:** La función `get` se utiliza para obtener el valor asociado a una clave en un mapa. En este caso, la clave es "dos". La función devuelve el valor asociado a la clave, que es 2.

8. **Imprimir el valor obtenido en la consola:** La función `println` se utiliza para imprimir un mensaje en la consola. En este caso, se imprime un mensaje con el valor obtenido del mapa.

9. **Función que recibe dos números enteros y devuelve su suma:** La función `sumar` es una función que recibe dos números enteros y devuelve su suma.

10. **Llamar a la función sumar y almacenar el resultado en una variable:** La función `sumar` se llama con los argumentos 3 y 5. El resultado de la función se almacena en la variable `resultado`.

11. **Imprimir el resultado en la consola:** La función `println` se utiliza para imprimir un mensaje en la consola. En este caso, se imprime un mensaje con el resultado de la función sumar.

12. **Clase de persona:** La clase `Persona` es una clase que representa a una persona. Tiene dos propiedades: `nombre` y `edad`.

13. **Crear una instancia de la clase persona:** La clase `Persona` se instancia con los argumentos "Juan" y 20. Esto crea un objeto de la clase `Persona`.

14. **Imprimir el nombre de la persona en la consola:** La propiedad `nombre` del objeto `persona` se imprime en la consola.

15. **Imprimir la edad de la persona en la consola:** La propiedad `edad` del objeto `persona` se imprime en la consola.