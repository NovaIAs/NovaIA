```php
<?php

// Función personalizada para obtener el Factorial de un número
function factorial($n) {
    if ($n < 0) {
        throw new Exception("El valor de entrada debe ser positivo");
    }
    if ($n == 0) {
        return 1;
    } else {
        return $n * factorial($n - 1);
    }
}

// Función personalizada para obtener la media de los elementos de un array
function promedio($array) {
    if (count($array) == 0) {
        return 0;
    } else {
        $suma = array_sum($array);
        return $suma / count($array);
    }
}

// Función personalizada para obtener la mediana de los elementos de un array
function mediana($array) {
    $array_ordenado = sort($array);
    $longitud = count($array);
    if ($longitud % 2 == 0) {
        // Si la longitud es par, la mediana es la media de los dos valores centrales
        $mediana = ($array_ordenado[$longitud / 2 - 1] + $array_ordenado[$longitud / 2]) / 2;
    } else {
        // Si la longitud es impar, la mediana es el valor central
        $mediana = $array_ordenado[$longitud / 2];
    }
    return $mediana;
}

// Función personalizada para generar un array de 10 números aleatorios entre 1 y 100
function getArrayNumerosAleatorios() {
    $numeros = [];
    for ($i = 0; $i < 10; $i++) {
        $numeros[] = rand(1, 100);
    }
    return $numeros;
}

// Función personalizada para obtener el elemento más frecuente de un array
function elementoMasFrecuente($array) {
    $frecuencias = [];
    foreach ($array as $elemento) {
        if (isset($frecuencias[$elemento])) {
            $frecuencias[$elemento]++;
        } else {
            $frecuencias[$elemento] = 1;
        }
    }
    $elementoMasFrecuente = '';
    $frecuenciaMaxima = 0;
    foreach ($frecuencias as $elemento => $frecuencia) {
        if ($frecuencia > $frecuenciaMaxima) {
            $elementoMasFrecuente = $elemento;
            $frecuenciaMaxima = $frecuencia;
        }
    }
    return $elementoMasFrecuente;
}

// Función personalizada para eliminar los elementos duplicados de un array
function eliminarDuplicados($array) {
    return array_unique($array);
}

// Función personalizada para obtener el producto de los elementos de un array
function producto($array) {
    if (count($array) == 0) {
        return 1;
    } else {
        return $array[0] * producto(array_slice($array, 1));
    }
}

// Función personalizada para obtener la suma de los elementos de un array
function suma($array) {
    if (count($array) == 0) {
        return 0;
    } else {
        return $array[0] + suma(array_slice($array, 1));
    }
}

// Función personalizada para invertir el orden de los elementos de un array
function invertir($array) {
    $array_inverso = [];
    for ($i = count($array) - 1; $i >= 0; $i--) {
        $array_inverso[] = $array[$i];
    }
    return $array_inverso;
}

// Ejemplo de uso de las funciones personalizadas
$numeros = getArrayNumerosAleatorios();
echo "Array de 10 números aleatorios entre 1 y 100: ";
print_r($numeros);

$factorial = factorial(5);
echo "\nFactorial de 5: $factorial\n";

$media = promedio($numeros);
echo "Media de los elementos del array: $media\n";

$mediana = mediana($numeros);
echo "Mediana de los elementos del array: $mediana\n";

$elementoMasFrecuente = elementoMasFrecuente($numeros);
echo "Elemento más frecuente del array: $elementoMasFrecuente\n";

$numeros_sin_duplicados = eliminarDuplicados($numeros);
echo "Array sin elementos duplicados: ";
print_r($numeros_sin_duplicados);

$producto = producto($numeros);
echo "\nProducto de los elementos del array: $producto\n";

$suma = suma($numeros);
echo "Suma de los elementos del array: $suma\n";

$array_inverso = invertir($numeros);
echo "Array invertido: ";
print_r($array_inverso);

?>
```

Este código es un conjunto de funciones personalizadas para realizar diversas operaciones con arrays y números. Las funciones incluyen el cálculo del factorial de un número, la obtención de la media y la mediana de un array, encontrar el elemento más frecuente, eliminar elementos duplicados, obtener el producto y la suma de los elementos, e invertir el orden de los elementos de un array.

El código también incluye un ejemplo de uso de las funciones personalizadas para generar un array de números aleatorios, calcular el factorial de un número y obtener la media, mediana, elemento más frecuente, elementos sin duplicados, producto, suma y array invertido de un array de números aleatorios.