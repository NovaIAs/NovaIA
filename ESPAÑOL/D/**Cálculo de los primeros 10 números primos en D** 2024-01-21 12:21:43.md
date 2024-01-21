```d
// Un programa en D que calcula los primeros 10 números primos

// Importamos la biblioteca estándar para usar las funciones matemáticas
import std.stdio, std.math;

// Creamos una función que comprueba si un número es primo
bool esPrimo(in int n) {
    // Comprobamos si el número es menor o igual que 1
    if (n <= 1) {
        return false;
    }

    // Comprobamos si el número es divisible por alguno de los números impares menores que su raíz cuadrada
    for (int i = 3; i * i <= n; i += 2) {
        if (n % i == 0) {
            return false;
        }
    }

    // Si el número no es divisible por ninguno de los números impares menores que su raíz cuadrada, entonces es primo
    return true;
}

// Creamos una función que calcula los primeros n números primos
int[] primerosPrimos(in int n) {
    // Creamos un array para almacenar los números primos
    int[] primos = new int[n];

    // Inicializamos el contador de números primos a 0
    int contador = 0;

    // Comenzamos a buscar números primos a partir del número 2
    int i = 2;

    // Mientras no hayamos encontrado n números primos, continuamos buscando
    while (contador < n) {
        // Si el número es primo, lo añadimos al array de números primos
        if (esPrimo(i)) {
            primos[contador] = i;
            contador++;
        }

        // Pasamos al siguiente número impar
        i += 2;
    }

    // Devolvemos el array de números primos
    return primos;
}

// Creamos una función que imprime los primeros n números primos
void imprimirPrimerosPrimos(in int n) {
    // Calculamos los primeros n números primos
    int[] primos = primerosPrimos(n);

    // Imprimimos los números primos
    for (int i = 0; i < n; i++) {
        writefln("%d", primos[i]);
    }
}

// Llamamos a la función que imprime los primeros 10 números primos
imprimirPrimerosPrimos(10);
```

Este programa es más complejo que el anterior porque utiliza funciones más avanzadas y hace uso de la biblioteca estándar de D para realizar algunas operaciones.

En primer lugar, se importa la biblioteca estándar de D, que proporciona una serie de funciones y tipos de datos útiles.

A continuación, se define una función llamada `esPrimo` que comprueba si un número es primo. Esta función toma un número entero como argumento y devuelve un valor booleano que indica si el número es primo o no.

La función `esPrimo` funciona comprobando si el número es divisible por alguno de los números impares menores que su raíz cuadrada. Si el número es divisible por alguno de estos números, entonces no es primo. Si el número no es divisible por ninguno de estos números, entonces es primo.

A continuación, se define una función llamada `primerosPrimos` que calcula los primeros n números primos. Esta función toma un número entero como argumento y devuelve un array de números enteros que contiene los primeros n números primos.

La función `primerosPrimos` funciona llamando a la función `esPrimo` para comprobar si un número es primo. Si el número es primo, se añade al array de números primos. Si el número no es primo, se pasa al siguiente número impar.

Por último, se define una función llamada `imprimirPrimerosPrimos` que imprime los primeros n números primos. Esta función toma un número entero como argumento y no devuelve ningún valor.

La función `imprimirPrimerosPrimos` funciona llamando a la función `primerosPrimos` para calcular los primeros n números primos. A continuación, imprime los números primos en la consola.

Este programa es un ejemplo de cómo se pueden utilizar las funciones y los arrays en D para realizar tareas complejas.