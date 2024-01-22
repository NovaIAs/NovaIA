```d
import std.algorithm, std.range, std.stdio, std.string, std.unittest;

static void printList(T)(in T[] arr) {
    foreach (item; arr) {
        writeln(item);
    }
}

template static void bubbleSort(T)(in out T[] arr) {
    enum sorted = true;
    while (sorted) {
        sorted = false;
        for (size_t i = 0; i < arr.length - 1; i++) {
            if (arr[i] > arr[i + 1]) {
                T temp = arr[i];
                arr[i] = arr[i + 1];
                arr[i + 1] = temp;
                sorted = true;
            }
        }
    }
}

void main() {
    int[] arr = [1, 5, 3, 2, 4];
    printList(arr);
    bubbleSort(arr);
    printList(arr);

    unittest {
        int[] arr = [6, 3, 1, 8, 7, 5, 2, 4];
        int[] sortedArr = [1, 2, 3, 4, 5, 6, 7, 8];
        bubbleSort(arr);
        assert(arr == sortedArr);
    }
}
```

Explicación:

1. **Ordenamiento Burbuja**: El código implementa el algoritmo de ordenamiento burbuja en D. El algoritmo compara cada par de elementos adyacentes en una lista y los intercambia si están en el orden incorrecto. Este proceso se repite hasta que no se realicen más intercambios, lo que significa que la lista está ordenada.

2. **Función `printList`**: Esta función se utiliza para imprimir los elementos de una lista en la consola. Recibe un array de tipo `T` como argumento y recorre cada elemento del array, imprimiendo su valor en una nueva línea.

3. **Función `bubbleSort`**: Esta función implementa el algoritmo de ordenamiento burbuja. Recibe un array de tipo `T` como argumento y lo ordena in situ. Utiliza una variable `sorted` para determinar si la lista ya está ordenada, y un bucle `while` para recorrer la lista repetidamente hasta que esté completamente ordenada.

4. **Función `main`**: La función `main` es el punto de entrada del programa. Aquí creamos un array de enteros, lo imprimimos en la consola, lo ordenamos usando la función `bubbleSort` y luego lo imprimimos nuevamente para mostrar el resultado ordenado.

5. **Pruebas unitarias**: El código también incluye un conjunto de pruebas unitarias para verificar si la función `bubbleSort` funciona correctamente. Las pruebas utilizan la función `assert` para comparar el array ordenado con el array esperado. Si las pruebas pasan, el programa se ejecuta sin errores.