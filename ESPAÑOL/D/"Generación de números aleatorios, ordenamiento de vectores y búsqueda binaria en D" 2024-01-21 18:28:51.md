```d
// Creación de un vector de números aleatorios
import std.random;

void main() {
    Random rand; // Generador de números aleatorios
    int[] vec = new int[10]; // Vector de 10 enteros

    // Llenado del vector con números aleatorios entre 1 y 100
    for (int i = 0; i < vec.length; i++) {
        vec[i] = rand.nextInt(1, 101);
    }

    // Impresión del vector
    for (int num : vec) {
        writefln("%d", num);
    }

    // Ordenamiento del vector usando el algoritmo de la burbuja
    for (int i = 0; i < vec.length - 1; i++) {
        for (int j = 0; j < vec.length - i - 1; j++) {
            if (vec[j] > vec[j + 1]) {
                int temp = vec[j];
                vec[j] = vec[j + 1];
                vec[j + 1] = temp;
            }
        }
    }

    // Impresión del vector ordenado
    writefln("Vector ordenado:");
    for (int num : vec) {
        writefln("%d", num);
    }

    // Búsqueda binaria de un número en el vector ordenado
    int target = 50;
    int low = 0;
    int high = vec.length - 1;
    int index = -1;

    while (low <= high) {
        int mid = (low + high) ~/ 2;
        if (vec[mid] == target) {
            index = mid;
            break;
        }
        else if (vec[mid] < target) {
            low = mid + 1;
        }
        else {
            high = mid - 1;
        }
    }

    if (index == -1) {
        writefln("El número %d no se encuentra en el vector.", target);
    }
    else {
        writefln("El número %d se encuentra en el índice %d del vector.", target, index);
    }
}
```

Explicación del código:

1. Se importa la biblioteca `std.random` que proporciona funciones para generar números aleatorios.

2. Se define una función `main` que es el punto de entrada del programa.

3. Se crea un generador de números aleatorios llamado `rand` y un vector llamado `vec` de 10 enteros.

4. Se llena el vector con números aleatorios entre 1 y 100 usando un bucle `for`.

5. Se imprime el vector usando otro bucle `for`.

6. Se ordena el vector usando el algoritmo de la burbuja. Este algoritmo itera sobre el vector y compara cada elemento con el siguiente, intercambiándolos si están en el orden incorrecto.

7. Se imprime el vector ordenado.

8. Se realiza una búsqueda binaria para encontrar un número en el vector ordenado. La búsqueda binaria divide el vector en dos mitades y busca el número en la mitad correspondiente. Este proceso se repite hasta encontrar el número o determinar que no está en el vector.

9. Se imprime el resultado de la búsqueda binaria.