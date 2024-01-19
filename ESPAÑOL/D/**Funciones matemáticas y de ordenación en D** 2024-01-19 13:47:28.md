```d
// Importamos las librerías necesarias
import std.stdio, std.algorithm, std.range, std.container;
import std.bigint, std.traits, std.math;

// Declaramos una función para calcular el factorial de un número
real factorial(real n) {
    if (n <= 1) {
        return 1;
    }

    return n * factorial(n - 1);
}

// Declaramos una función para calcular el máximo común divisor de dos números.
real mcd(real a, real b) {
    while (b != 0) {
        var temp = b;
        b = a % b;
        a = temp;
    }

    return a.abs;
}

// Declaramos una función para calcular el mínimo común múltiplo de dos números.
real mcm(real a, real b) {
    return (a * b).abs / mcd(a, b);
}

// Declaramos una función para encontrar el número máximo en un rango de números.
real max(real[] arr) {
    var max = arr[0];
    for (i in 1..arr.length) {
        if (arr[i] > max) {
            max = arr[i];
        }
    }

    return max;
}

// Declaramos una función para encontrar el número mínimo en un rango de números.
real min(real[] arr) {
    var min = arr[0];
    for (i in 1..arr.length) {
        if (arr[i] < min) {
            min = arr[i];
        }
    }

    return min;
}

// Declaramos una función para invertir un rango de números.
void reverse(real[] arr) {
    var i = 0;
    var j = arr.length - 1;

    while (i < j) {
        var temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;

        i++;
        j--;
    }
}

// Declaramos una función para ordenar un rango de números.
void sort(real[] arr) {
    arr.sort;
}

// Declaramos una función para encontrar el índice de un elemento en un rango de números.
int findIndex(real[] arr, real element) {
    for (i in 0..arr.length) {
        if (arr[i] == element) {
            return i;
        }
    }

    return -1;
}

void main() {
    var a = factorial(5);
    writefln("El factorial de 5 es %d", a);

    var b = mcd(12, 18);
    writefln("El máximo común divisor de 12 y 18 es %d", b);

    var c = mcm(12, 18);
    writefln("El mínimo común múltiplo de 12 y 18 es %d", c);

    var arr = [1, 6, 3, 2, 8];
    var maxVal = max(arr);
    writefln("El valor máximo en %s es %d", arr, maxVal);

    var minVal = min(arr);
    writefln("El valor mínimo en %s es %d", arr, minVal);

    reverse(arr);
    writefln("El rango invertido %s es %s", arr, arr);

    sort(arr);
    writefln("El rango ordenado %s es %s", arr, arr);

    var index = findIndex(arr, 3);
    if (index == -1) {
        writefln("El elemento 3 no se encontró en el rango");
    } else {
        writefln("El elemento 3 se encontró en el índice %d del rango", index);
    }
}
```

Explicación del código:

1. La función `factorial` calcula el factorial de un número.
2. La función `mcd` calcula el máximo común divisor de dos números.
3. La función `mcm` calcula el mínimo común múltiplo de dos números.
4. La función `max` encuentra el número máximo en un rango de números.
5. La función `min` encuentra el número mínimo en un rango de números.
6. La función `reverse` invierte un rango de números.
7. La función `sort` ordena un rango de números.
8. La función `findIndex` encuentra el índice de un elemento en un rango de números.

La función `main` es la función principal del programa. Primero llama a la función `factorial` para calcular el factorial de 5. Luego llama a la función `mcd` para calcular el máximo común divisor de 12 y 18. Luego llama a la función `mcm` para calcular el mínimo común múltiplo de 12 y 18.

Luego, crea un rango de números [1, 6, 3, 2, 8] y llama a la función `max` para encontrar el número máximo en el rango. Luego llama a la función `min` para encontrar el número mínimo en el rango. Luego llama a la función `reverse` para invertir el rango. Luego llama a la función `sort` para ordenar el rango.

Finalmente, llama a la función `findIndex` para encontrar el índice del elemento 3 en el rango. Si el elemento 3 no se encuentra en el rango, imprime un mensaje. De lo contrario, imprime el índice del elemento 3 en el rango.