```php
// Función principal
function main() {
    // Inicializar variables
    $numero1 = 10;
    $numero2 = 20;

    // Calcular el máximo común divisor (MCD) utilizando el algoritmo de Euclides
    $mcd = mcd($numero1, $numero2);

    // Calcular el mínimo común múltiplo (MCM) utilizando la fórmula MCD * (número1 / MCD) * (número2 / MCD)
    $mcm = mcm($mcd, $numero1, $numero2);

    // Mostrar el resultado
    echo "El máximo común divisor (MCD) de $numero1 y $numero2 es $mcd <br>";
    echo "El mínimo común múltiplo (MCM) de $numero1 y $numero2 es $mcm <br>";
}

// Función para calcular el máximo común divisor (MCD)
function mcd($numero1, $numero2) {
    // Si el segundo número es 0, entonces el MCD es el primer número
    if ($numero2 == 0) {
        return $numero1;
    }

    // De lo contrario, llamar a la función recursivamente con el segundo número y el resto de la división del primer número entre el segundo número
    else {
        return mcd($numero2, $numero1 % $numero2);
    }
}

// Función para calcular el mínimo común múltiplo (MCM)
function mcm($mcd, $numero1, $numero2) {
    // Calcular el MCM utilizando la fórmula MCD * (número1 / MCD) * (número2 / MCD)
    return ($mcd * ($numero1 / $mcd) * ($numero2 / $mcd));
}

// Llamar a la función principal
main();
```

Explicación del código:

1. La función `main()` es la función principal del programa. Esta función inicializa las variables `numero1`, `numero2`, calcula el MCD y el MCM utilizando las funciones `mcd()` y `mcm()`, y finalmente muestra el resultado en la consola.

2. La función `mcd()` calcula el MCD utilizando el algoritmo de Euclides. El algoritmo de Euclides es un método eficiente para calcular el MCD de dos números. El algoritmo funciona dividiendo el primer número entre el segundo número y tomando el resto. A continuación, se divide el segundo número por el resto, y así sucesivamente. El MCD es el último resto no nulo.

3. La función `mcm()` calcula el MCM utilizando la fórmula `MCM = MCD * (número1 / MCD) * (número2 / MCD)`. Esta fórmula se deriva del hecho de que el MCM es el menor número que es múltiplo tanto del primer número como del segundo número.

4. El programa llama a la función `main()` para ejecutar el programa.