```d
// Crear una función que reciba un entero y retorne su factorial
int factorial(int n) {
    if (n == 0) {
        return 1;
    }
    else {
        return n * factorial(n - 1);
    }
}

// Función principal del programa
void main() {
    // Imprimir los primeros 10 factoriales
    for (int i = 0; i < 10; i++) {
        writefln("$i! = $factorial(i)");
    }
}
```

Explicación:

* La función `factorial` es una función recursiva, es decir, una función que se invoca a sí misma. Esta función calcula el factorial de un número, que es el producto de todos los números enteros positivos desde 1 hasta ese número.
* La función `main` es la función principal del programa. En esta función se llama a la función `factorial` para calcular los primeros 10 factoriales y se imprimen los resultados en la consola.
* La instrucción `writefln` imprime una línea de texto en la consola, utilizando el formato especificado en el primer argumento. En este caso, el formato es `"$i! = $factorial(i)"`, donde `$i` es reemplazado por el valor de la variable `i` y `$factorial(i)` es reemplazado por el resultado de llamar a la función `factorial` con el argumento `i`.