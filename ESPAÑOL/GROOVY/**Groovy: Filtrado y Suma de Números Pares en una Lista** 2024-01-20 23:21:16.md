```groovy
//Groovy con muchas sentencias a la vez, en donde se manipulan arreglos y se determinan números filtrados y la suma de estos.

def numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

def numerosFiltrados = numeros.findAll { it % 2 == 0 }

def sumaNumerosFiltrados = numerosFiltrados.sum()

println "Los números pares en la lista son: ${numerosFiltrados}"
println "La suma de los números pares en la lista es: ${sumaNumerosFiltrados}"


// Implementación con otras sintaxis menos expresiva

def numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

def numerosFiltrados = []
numeros.each {
    if (it % 2 == 0) {
        numerosFiltrados.add(it)
    }
}

def sumaNumerosFiltrados = 0
numerosFiltrados.each {
    sumaNumerosFiltrados += it
}

println "Los números pares en la lista son: ${numerosFiltrados}"
println "La suma de los números pares en la lista es: ${sumaNumerosFiltrados}"

```

Explicación del código:

* Se define una lista de números llamada `numeros`.
* Se define una lista vacía llamada `numerosFiltrados` para almacenar los números pares de la lista `numeros`.
* Se utiliza la función `findAll` para filtrar los números pares de la lista `numeros` y almacenarlos en la lista `numerosFiltrados`.
* Se utiliza la función `sum` para sumar los números de la lista `numerosFiltrados` y almacenar el resultado en la variable `sumaNumerosFiltrados`.
* Se imprime en la consola los números pares de la lista `numeros` y la suma de estos números.

El código es complejo porque utiliza varias funciones y estructuras de datos diferentes. Sin embargo, es fácil de entender si se desglosa en pasos individuales.