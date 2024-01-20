```groovy
// Clase principal
class ProgramaPrincipal {

    // Método principal
    static void main(String[] args) {
        // Creamos una lista de números
        List<Integer> numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

        // Imprimimos la lista original
        println("Lista original:")
        println(numeros)

        // Creamos una lista de números pares
        List<Integer> pares = numeros.findAll { numero -> numero % 2 == 0 }

        // Imprimimos la lista de números pares
        println("Lista de números pares:")
        println(pares)

        // Creamos una lista de números impares
        List<Integer> impares = numeros.findAll { numero -> numero % 2 != 0 }

        // Imprimimos la lista de números impares
        println("Lista de números impares:")
        println(impares)

        // Creamos una lista de números mayores que 5
        List<Integer> mayoresQue5 = numeros.findAll { numero -> numero > 5 }

        // Imprimimos la lista de números mayores que 5
        println("Lista de números mayores que 5:")
        println(mayoresQue5)

        // Creamos una lista de números menores o iguales a 5
        List<Integer> menoresOIgualesA5 = numeros.findAll { numero -> numero <= 5 }

        // Imprimimos la lista de números menores o iguales a 5
        println("Lista de números menores o iguales a 5:")
        println(menoresOIgualesA5)

        // Creamos una lista de números que son múltiplos de 3
        List<Integer> multiplosDe3 = numeros.findAll { numero -> numero % 3 == 0 }

        // Imprimimos la lista de números que son múltiplos de 3
        println("Lista de números que son múltiplos de 3:")
        println(multiplosDe3)

        // Creamos una lista de números que no son múltiplos de 3
        List<Integer> noMultiplosDe3 = numeros.findAll { numero -> numero % 3 != 0 }

        // Imprimimos la lista de números que no son múltiplos de 3
        println("Lista de números que no son múltiplos de 3:")
        println(noMultiplosDe3)
    }
}
```

Este código groovy crea varias listas de números a partir de una lista original, utilizando la función `findAll` para filtrar los elementos de la lista original según ciertas condiciones.

* La lista `pares` contiene los números pares de la lista original.
* La lista `impares` contiene los números impares de la lista original.
* La lista `mayoresQue5` contiene los números mayores que 5 de la lista original.
* La lista `menoresOIgualesA5` contiene los números menores o iguales a 5 de la lista original.
* La lista `multiplosDe3` contiene los números que son múltiplos de 3 de la lista original.
* La lista `noMultiplosDe3` contiene los números que no son múltiplos de 3 de la lista original.

El código utiliza el operador `%` para calcular el resto de la división de un número entre otro. El operador `==` se utiliza para comparar dos valores. El operador `!=` se utiliza para comparar dos valores y devolver `true` si son diferentes.

El código también utiliza la función `println` para imprimir los resultados en la consola.