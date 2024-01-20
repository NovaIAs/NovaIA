```d
// Modulo que calcula el máximo común divisor entre dos enteros positivos.

module MCD {
    export func maximoComunDivisor(a, b: int): int pure nothrow @safe {
        if (b == 0)
            return a;
        return maximoComunDivisor(b, a % b);
    }

    export void main() {
        writeln("El máximo común divisor de 12 y 18 es:", maximoComunDivisor(12, 18));
        writeln("El máximo común divisor de 21 y 14 es:", maximoComunDivisor(21, 14));
    }
}
```

Explicación:

* El módulo `MCD` define una función llamada `maximoComunDivisor` que calcula el máximo común divisor entre dos enteros positivos.
* La función `maximoComunDivisor` utiliza el algoritmo de Euclides para calcular el máximo común divisor.
* El algoritmo de Euclides funciona dividiendo los dos números entre sí y tomando el resto de la división.
* El máximo común divisor es el último resto que no es cero.
* La función `maximoComunDivisor` se llama con los dos números cuyos máximo común divisor queremos calcular y devuelve el máximo común divisor.
* La función `main` llama a la función `maximoComunDivisor` con dos pares de números y escribe los resultados en la consola.