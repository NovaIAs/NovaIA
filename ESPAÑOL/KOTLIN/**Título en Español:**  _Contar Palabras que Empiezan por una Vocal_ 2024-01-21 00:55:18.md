```kotlin
// Programa que lee un texto y cuenta las palabras que empiezan por una vocal

// Función principal del programa
fun main(args: Array<String>) {
    // Pedimos al usuario que introduzca un texto
    print("Introduce un texto: ")
    val texto = readLine()!!

    // Convertimos el texto a minúsculas y lo dividimos en palabras
    val palabras = texto.toLowerCase().split(" ")

    // Creamos una lista para almacenar las palabras que empiezan por una vocal
    val palabrasConVocal = mutableListOf<String>()

    // Recorremos las palabras y añadimos las que empiezan por una vocal a la lista
    for (palabra in palabras) {
        if (palabra[0] in "aeiou") {
            palabrasConVocal.add(palabra)
        }
    }

    // Imprimimos el número de palabras que empiezan por una vocal
    println("El texto contiene ${palabrasConVocal.size} palabras que empiezan por una vocal")

    // Imprimimos las palabras que empiezan por una vocal
    for (palabra in palabrasConVocal) {
        println(palabra)
    }
}
```

**Explicación del código:**

1. La función `main` es la función principal del programa y es la que se ejecuta cuando se ejecuta el programa.

2. La variable `texto` almacena el texto introducido por el usuario.

3. La variable `palabras` almacena las palabras del texto divididas en una lista.

4. La variable `palabrasConVocal` almacena las palabras de la lista `palabras` que empiezan por una vocal.

5. El ciclo `for` recorre la lista `palabras` y añade las palabras que empiezan por una vocal a la lista `palabrasConVocal`.

6. El código `println(palabrasConVocal.size)` imprime el número de palabras que empiezan por una vocal.

7. El ciclo `for` recorre la lista `palabrasConVocal` e imprime cada una de las palabras.