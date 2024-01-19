```groovy
// Definición de la clase Principal
class Principal {

    // Definición del método mostrarArreglo
    static mostrarArreglo(arreglo) {
        println("Elementos del arreglo:")
        for (elemento in arreglo) {
            println(elemento)
        }
    }

    // Definición del método buscarElemento
    static buscarElemento(arreglo, elementoBuscar) {
        for (i in 0..<arreglo.size()) {
            if (arreglo[i] == elementoBuscar) {
                return i
            }
        }
        return -1
    }

    // Definición del método ordenarArreglo
    static ordenarArreglo(arreglo) {
        for (i in 0..<arreglo.size()) {
            for (j in (i + 1)..<arreglo.size()) {
                if (arreglo[i] > arreglo[j]) {
                    temp = arreglo[i]
                    arreglo[i] = arreglo[j]
                    arreglo[j] = temp
                }
            }
        }
        return arreglo
    }

    // Definición del método main
    static main(args) {

        // Creación de un arreglo
        arreglo = [1, 3, 5, 2, 4]

        // Invocación del método mostrarArreglo
        mostrarArreglo(arreglo)

        // Invocación del método buscarElemento
        elementoBuscar = 3
        indiceElemento = buscarElemento(arreglo, elementoBuscar)
        if (indiceElemento != -1) {
            println("El elemento $elementoBuscar se encuentra en el índice $indiceElemento")
        } else {
            println("El elemento $elementoBuscar no se encuentra en el arreglo")
        }

        // Invocación del método ordenarArreglo
        arregloOrdenado = ordenarArreglo(arreglo)

        // Invocación del método mostrarArreglo
        mostrarArreglo(arregloOrdenado)
    }
}
```

Explicación del código:

- Se define la clase `Principal`.

- En la clase `Principal`, se definen los siguientes métodos:
    - `mostrarArreglo(arreglo)`: Este método muestra los elementos de un arreglo.
    - `buscarElemento(arreglo, elementoBuscar)`: Este método busca un elemento en un arreglo y devuelve su índice si lo encuentra. De lo contrario, devuelve -1.
    - `ordenarArreglo(arreglo)`: Este método ordena un arreglo en orden ascendente y devuelve el arreglo ordenado.

- En el método `main`, se crean un arreglo de números enteros, se invoca el método `mostrarArreglo` para mostrar los elementos del arreglo, se invoca el método `buscarElemento` para buscar el elemento `3` en el arreglo, se invoca el método `ordenarArreglo` para ordenar el arreglo y se invoca nuevamente el método `mostrarArreglo` para mostrar los elementos del arreglo ordenado.