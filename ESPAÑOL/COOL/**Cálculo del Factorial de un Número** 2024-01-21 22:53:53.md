```cool
-- Un programa que calcula el factorial de un número.

-- Declaración de la función principal.
entero main() {
    -- Declaración de variables.
    entero n;

    -- Lectura del número.
    entero leer_numero() {
        entero n;
        escribir("Ingrese un número: ");
        leer(n);
        return n;
    }

    -- Cálculo del factorial.
    entero factorial(entero n) {
        entero resultado = 1;

        mientras (n > 0) {
            resultado *= n;
            n--;
        }

        return resultado;
    }

    -- Impresión del resultado.
    escribir("El factorial de " + convertir_a_cadena(n) + " es " + convertir_a_cadena(factorial(n)));

    -- Fin de la función principal.
}
```

Explicación:

* La función `main` es la función principal del programa. Es la primera función que se ejecuta.
* La función `leer_numero` lee un número del usuario y lo devuelve.
* La función `factorial` calcula el factorial de un número.
* La función `convertir_a_cadena` convierte un número a una cadena.
* La función `escribir` imprime un mensaje en la consola.