```d
import std.stdio, std.algorithm, std.range, std.string, std.traits;

// Definir una función para imprimir una lista de números
void printList(ref ImmutableArray!int lista) {
    foreach (i, n; lista) {
        writefln("%d", n);
    }
}

// Definir una función para filtrar una lista de números y obtener sólo los números pares
ImmutableArray!int filterEvenNumbers(ref ImmutableArray!int lista) {
    ImmutableArray!int resultado = new ImmutableArray!int();
    foreach (i, n; lista) {
        if (n % 2 == 0) {
            resultado ~= n;
        }
    }
    return resultado;
}

// Definir una función para calcular el producto de una lista de números
int product(ref ImmutableArray!int lista) {
    int resultado = 1;
    foreach (i, n; lista) {
        resultado *= n;
    }
    return resultado;
}

// Definir una función para ordenar una lista de números en orden ascendente
ImmutableArray!int sortAscending(ref ImmutableArray!int lista) {
    ImmutableArray!int resultado = new ImmutableArray!int();
    lista.sort!(ascending);
    foreach (i, n; lista) {
        resultado ~= n;
    }
    return resultado;
}

// Definir una función para obtener el elemento máximo de una lista de números
int maxElement(ref ImmutableArray!int lista) {
    int max = lista[0];
    foreach (i, n; lista) {
        if (n > max) {
            max = n;
        }
    }
    return max;
}

// Definir una función para obtener el elemento mínimo de una lista de números
int minElement(ref ImmutableArray!int lista) {
    int min = lista[0];
    foreach (i, n; lista) {
        if (n < min) {
            min = n;
        }
    }
    return min;
}

// Definir una función para contar el número de veces que un elemento aparece en una lista
int countElement(ref ImmutableArray!int lista, int elemento) {
    int count = 0;
    foreach (i, n; lista) {
        if (n == elemento) {
            count++;
        }
    }
    return count;
}

// Definir una función para eliminar un elemento de una lista
ImmutableArray!int deleteElement(ref ImmutableArray!int lista, int elemento) {
    ImmutableArray!int resultado = new ImmutableArray!int();
    foreach (i, n; lista) {
        if (n != elemento) {
            resultado ~= n;
        }
    }
    return resultado;
}

// Definir una función para invertir una lista
ImmutableArray!int reverseList(ref ImmutableArray!int lista) {
    ImmutableArray!int resultado = new ImmutableArray!int();
    for (i = lista.length - 1; i >= 0; i--) {
        resultado ~= lista[i];
    }
    return resultado;
}

// Definir una función para obtener la media de una lista de números
float average(ref ImmutableArray!int lista) {
    float suma = 0;
    foreach (i, n; lista) {
        suma += n;
    }
    return suma / lista.length;
}

// Definir una función para obtener la desviación estándar de una lista de números
float standardDeviation(ref ImmutableArray!int lista) {
    float media = average(lista);
    float varianza = 0;
    foreach (i, n; lista) {
        varianza += pow(n - media, 2);
    }
    return sqrt(varianza / lista.length);
}

void main() {
    // Crear una lista de números
    ImmutableArray!int lista = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

    // Imprimir la lista original
    writefln("Lista original:");
    printList(lista);

    // Filtrar los números pares de la lista
    ImmutableArray!int numerosPares = filterEvenNumbers(lista);

    // Imprimir la lista de números pares
    writefln("Lista de números pares:");
    printList(numerosPares);

    // Calcular el producto de los números de la lista
    int producto = product(lista);

    // Imprimir el producto
    writefln("Producto:", producto);

    // Ordenar la lista en orden ascendente
    ImmutableArray!int listaOrdenada = sortAscending(lista);

    // Imprimir la lista ordenada
    writefln("Lista ordenada:");
    printList(listaOrdenada);

    // Obtener el elemento máximo de la lista
    int elementoMaximo = maxElement(lista);

    // Imprimir el elemento máximo
    writefln("Elemento máximo:", elementoMaximo);

    // Obtener el elemento mínimo de la lista
    int elementoMinimo = minElement(lista);

    // Imprimir el elemento mínimo
    writefln("Elemento mínimo:", elementoMinimo);

    // Contar el número de veces que aparece el número 5 en la lista
    int count = countElement(lista, 5);

    // Imprimir el número de veces que aparece el número 5
    writefln("Número de veces que aparece el número 5:", count);

    // Eliminar el elemento 5 de la lista
    ImmutableArray!int listaSin5 = deleteElement(lista, 5);

    // Imprimir la lista sin el elemento 5
    writefln("Lista sin el elemento 5:");
    printList(listaSin5);

    // Invertir la lista
    ImmutableArray!int listaInvertida = reverseList(lista);

    // Imprimir la lista invertida
    writefln("Lista invertida:");
    printList(listaInvertida);

    // Obtener la media de la lista
    float media = average(lista);

    // Imprimir la media
    writefln("Media:", media);

    // Obtener la desviación estándar de la lista
    float desviacionEstandar = standardDeviation(lista);

    // Imprimir la desviación estándar
    writefln("Desviación estándar:", desviacionEstandar);
}
```