```swift
// Este código es un ejemplo de una función compleja que implementa el algoritmo de búsqueda binaria en un array ordenado.

func busquedaBinaria(array: [Int], elemento: Int) -> Int? {
    // Definimos los límites de la búsqueda.
    var izquierda = 0
    var derecha = array.count - 1

    // Mientras los límites no se crucen...
    while izquierda <= derecha {
        // Calculamos el índice medio del array.
        let medio = (izquierda + derecha) / 2

        // Si el elemento es igual al elemento en el índice medio...
        if array[medio] == elemento {
            // Devolvemos el índice medio.
            return medio
        }
        // Si el elemento es menor que el elemento en el índice medio...
        else if array[medio] > elemento {
            // Ajustamos el límite derecho al índice medio menos uno.
            derecha = medio - 1
        }
        // Si el elemento es mayor que el elemento en el índice medio...
        else {
            // Ajustamos el límite izquierdo al índice medio más uno.
            izquierda = medio + 1
        }
    }

    // Si el elemento no se encontró, devolvemos nil.
    return nil
}

// Ejemplo de uso de la función busquedaBinaria.
let array = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
let elemento = 11

if let indice = busquedaBinaria(array: array, elemento: elemento) {
    print("El elemento \(elemento) se encuentra en el índice \(indice) del array.")
} else {
    print("El elemento \(elemento) no se encuentra en el array.")
}
```

Explicación del código:

* La función `busquedaBinaria` toma dos parámetros: un array ordenado y un elemento a buscar.
* La función inicializa dos variables, `izquierda` y `derecha`, que representan los límites de la búsqueda.
* El bucle `while` itera hasta que los límites se crucen.
* En cada iteración, el índice medio del array se calcula y se compara el elemento en ese índice con el elemento a buscar.
* Si el elemento es igual al elemento en el índice medio, se devuelve el índice medio.
* Si el elemento es menor que el elemento en el índice medio, el límite derecho se ajusta al índice medio menos uno.
* Si el elemento es mayor que el elemento en el índice medio, el límite izquierdo se ajusta al índice medio más uno.
* Si el elemento no se encontró, se devuelve `nil`.

El ejemplo de uso de la función `busquedaBinaria` muestra cómo buscar el elemento 11 en el array ordenado. Si el elemento se encuentra, se imprime su índice en el array. De lo contrario, se imprime un mensaje indicando que el elemento no se encontró.