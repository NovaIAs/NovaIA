```php
<?php

// Definir una función para calcular el factorial de un número
function factorial($numero) {
    if ($numero < 0) {
        throw new Exception("No se puede calcular el factorial de un número negativo.");
    }

    if ($numero == 0) {
        return 1;
    }

    $factorial = 1;
    for ($i = 1; $i <= $numero; $i++) {
        $factorial *= $i;
    }

    return $factorial;
}

// Definir una función para calcular la combinación de dos números
function combinacion($n, $r) {
    if ($n < $r) {
        throw new Exception("El número de elementos (n) debe ser mayor o igual que el número de elementos a seleccionar (r).");
    }

    if ($r == 0 || $r == $n) {
        return 1;
    }

    $combinacion = factorial($n) / (factorial($r) * factorial($n - $r));

    return $combinacion;
}

// Definir una función para calcular la permutación de dos números
function permutacion($n, $r) {
    if ($n < $r) {
        throw new Exception("El número de elementos (n) debe ser mayor o igual que el número de elementos a seleccionar (r).");
    }

    if ($r == 0) {
        return 1;
    }

    $permutacion = factorial($n) / factorial($n - $r);

    return $permutacion;
}

// Definir una función para calcular la probabilidad de un evento
function probabilidad($n, $x) {
    if ($n <= 0) {
        throw new Exception("El número de experimentos (n) debe ser mayor que cero.");
    }

    if ($x < 0 || $x > $n) {
        throw new Exception("El número de éxitos (x) debe estar entre 0 y n.");
    }

    $probabilidad = combinacion($n, $x) * pow(1/2, $n);

    return $probabilidad;
}

// Definir una función para calcular la media de una lista de números
function media($numeros) {
    if (empty($numeros)) {
        throw new Exception("La lista de números no puede estar vacía.");
    }

    $suma = 0;
    foreach ($numeros as $numero) {
        $suma += $numero;
    }

    $media = $suma / count($numeros);

    return $media;
}

// Definir una función para calcular la desviación estándar de una lista de números
function desviacion_estandar($numeros) {
    if (empty($numeros)) {
        throw new Exception("La lista de números no puede estar vacía.");
    }

    $media = media($numeros);

    $suma_cuadrados = 0;
    foreach ($numeros as $numero) {
        $suma_cuadrados += pow($numero - $media, 2);
    }

    $desviacion_estandar = sqrt($suma_cuadrados / (count($numeros) - 1));

    return $desviacion_estandar;
}

?>
```

Este código complejo en PHP contiene varias funciones matemáticas que pueden ser útiles para realizar cálculos estadísticos y combinatorios. Las funciones se explican a continuación:

* `factorial()`: calcula el factorial de un número.
* `combinacion()`: calcula la combinación de dos números.
* `permutacion()`: calcula la permutación de dos números.
* `probabilidad()`: calcula la probabilidad de un evento.
* `media()`: calcula la media de una lista de números.
* `desviacion_estandar()`: calcula la desviación estándar de una lista de números.

Para utilizar estas funciones, simplemente debes llamarlas desde tu código y proporcionarles los argumentos necesarios. Por ejemplo, para calcular el factorial de 5, puedes utilizar el siguiente código:

```php
$factorial = factorial(5);

echo "El factorial de 5 es: $factorial";
```

Esto imprimirá el siguiente resultado:

```
El factorial de 5 es: 120
```

De manera similar, puedes utilizar las otras funciones para realizar cálculos estadísticos y combinatorios.