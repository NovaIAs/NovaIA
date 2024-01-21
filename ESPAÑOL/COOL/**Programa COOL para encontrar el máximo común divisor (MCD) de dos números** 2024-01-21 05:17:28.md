```cool
/*
Programa COOL para encontrar el máximo común divisor (MCD) de dos números.

El MCD es el número más grande que divide a ambos números sin dejar resto. Por ejemplo, el MCD de 12 y 18 es 6.

El algoritmo de Euclides es un método eficiente para encontrar el MCD de dos números. Funciona encontrando el resto de la división del número mayor entre el número menor. El resto es el nuevo número mayor. Se repite el proceso hasta que el resto sea 0. El último número mayor es el MCD de los dos números originales.

*/

clase mcd {
    método principal() {
        entero a, b;
        escribir("Introduzca el primer número:");
        leer(a);
        escribir("Introduzca el segundo número:");
        leer(b);
        entero mcd = euclides(a, b);
        escribir("El MCD de", a, "y", b, "es", mcd);
    }

    método entero euclides(entero a, entero b) {
        si(b == 0) {
            devolver a;
        } else {
            devolver euclides(b, a % b);
        }
    }
}
```

Explicación del código:

* La clase `mcd` define el método `principal` y el método `euclides`.
* El método `principal` solicita al usuario que introduzca dos números y luego llama al método `euclides` para encontrar el MCD de los dos números.
* El método `euclides` implementa el algoritmo de Euclides para encontrar el MCD de dos números.
* El método `euclides` es recursivo, es decir, se llama a sí mismo.
* El método `euclides` utiliza el operador `%` para encontrar el resto de la división del número mayor entre el número menor.
* El método `euclides` devuelve el MCD de los dos números.