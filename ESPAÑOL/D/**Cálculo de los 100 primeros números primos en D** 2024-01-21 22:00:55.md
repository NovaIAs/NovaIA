```d
import std.stdio, std.algorithm, std.math;

// Programa que calcula los primeros 100 números primos.

void main() {
    // Crear una lista para almacenar los números primos.
    auto primos = new ArrayList~int!();

    // Agregar el 2 a la lista de números primos.
    primos ~= 2;

    // Crear una variable para almacenar el número actual que se está probando.
    auto n = 3;

    // Mientras no se hayan calculado 100 números primos, seguir probando números.
    while (primos.length < 100) {
        // Crear una variable para almacenar si el número actual es primo.
        auto esPrimo = true;

        // Recorrer la lista de números primos ya encontrados.
        foreach (p; primos) {
            // Si el número actual es divisible por alguno de los números primos ya encontrados, entonces no es primo.
            if (n % p == 0) {
                esPrimo = false;
                break;
            }
        }

        // Si el número actual es primo, agregarlo a la lista de números primos.
        if (esPrimo) {
            primos ~= n;
        }

        // Incrementar el número actual en 1.
        n++;
    }

    // Imprimir la lista de números primos.
    foreach (p; primos) {
        writefln("%d", p);
    }
}
```

Este código es más complejo que el anterior porque utiliza técnicas más avanzadas de programación en D.

En primer lugar, se utiliza la sentencia `foreach` para recorrer la lista de números primos ya encontrados y comprobar si el número actual es primo.

En segundo lugar, se utiliza la variable `esPrimo` para almacenar si el número actual es primo.

En tercer lugar, se utiliza la sentencia `break` para salir del bucle `foreach` cuando se encuentra un divisor del número actual.

En cuarto lugar, se utiliza la sentencia `continue` para continuar con la siguiente iteración del bucle `while`.

En quinto lugar, se utiliza la función `writefln` para imprimir el número actual en la consola.

Este código es un ejemplo de cómo se pueden utilizar las técnicas avanzadas de programación en D para escribir código complejo y eficiente.