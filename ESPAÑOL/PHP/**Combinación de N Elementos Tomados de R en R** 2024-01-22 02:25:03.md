```php
<?php

// Definir una función para calcular el factorial de un número.
function factorial($numero) {
    if ($numero < 0) {
        throw new InvalidArgumentException("El número debe ser positivo.");
    }

    if ($numero == 0) {
        return 1;
    } else {
        return $numero * factorial($numero - 1);
    }
}

// Definir una función para calcular la combinación de n elementos tomados de r en r.
function combinacion($n, $r) {
    if ($n < $r) {
        throw new InvalidArgumentException("n debe ser mayor que o igual que r.");
    }

    return factorial($n) / (factorial($r) * factorial($n - $r));
}

// Solicitar al usuario que introduzca el valor de n y r.
echo "Introduzca el valor de n: ";
$n = trim(fgets(STDIN));

echo "Introduzca el valor de r: ";
$r = trim(fgets(STDIN));

// Calcular el valor de la combinación.
try {
    $combinacion = combinacion($n, $r);

    // Mostrar el resultado.
    echo "La combinación de $n elementos tomados de $r en r es $combinacion.\n";
} catch (InvalidArgumentException $e) {
    // Mostrar el mensaje de error.
    echo "Error: " . $e->getMessage() . "\n";
}

?>
```

Explicación del código:

* La función `factorial()` calcula el factorial de un número.
* La función `combinacion()` calcula la combinación de n elementos tomados de r en r.
* El código solicita al usuario que introduzca el valor de n y r.
* El código llama a la función `combinacion()` para calcular el valor de la combinación.
* El código muestra el resultado en la consola.
* El código maneja las excepciones que puedan surgir al calcular la combinación.