```php
// Crear una función recursiva para calcular el factorial de un número.
function factorial($numero) {
    if ($numero == 0) {
        return 1;
    } else {
        return $numero * factorial($numero - 1);
    }
}

// Crear una función para calcular el número de combinaciones de una lista de elementos.
function combinaciones($elementos, $seleccionados) {
    if ($seleccionados == 0) {
        return 1;
    } elseif ($seleccionados > $elementos) {
        return 0;
    } else {
        return combinaciones($elementos - 1, $seleccionados - 1) + combinaciones($elementos - 1, $seleccionados);
    }
}

// Crear una función para generar todas las permutaciones de una lista de elementos.
function permutaciones($elementos) {
    if (count($elementos) == 1) {
        return [$elementos];
    } else {
        $permutaciones = [];
        for ($i = 0; $i < count($elementos); $i++) {
            $elemento = $elementos[$i];
            $restantes = array_slice($elementos, 0, $i) + array_slice($elementos, $i + 1);
            foreach (permutaciones($restantes) as $permutacion) {
                $permutaciones[] = array_merge([$elemento], $permutacion);
            }
        }
        return $permutaciones;
    }
}

// Crear una función para convertir un número decimal a binario.
function decimalToBinario($numero) {
    if ($numero == 0) {
        return "0";
    } else {
        return decimalToBinario(floor($numero / 2)) . ($numero % 2);
    }
}

// Crear una función para convertir un número binario a decimal.
function binarioToDecimal($numero) {
    if ($numero == 0) {
        return 0;
    } else {
        return (int)$numero % 10 + 2 * binarioToDecimal(floor($numero / 10));
    }
}

// Crear una función para encontrar el máximo común divisor de dos números.
function maximoComunDivisor($a, $b) {
    if ($b == 0) {
        return $a;
    } else {
        return maximoComunDivisor($b, $a % $b);
    }
}

// Crear una función para encontrar el mínimo común múltiplo de dos números.
function minimoComunMultiplo($a, $b) {
    return ($a * $b) / maximoComunDivisor($a, $b);
}

// Crear una función para encontrar todos los factores de un número.
function factores($numero) {
    $factores = [];
    for ($i = 1; $i <= sqrt($numero); $i++) {
        if ($numero % $i == 0) {
            $factores[] = $i;
            if ($i != $numero / $i) {
                $factores[] = $numero / $i;
            }
        }
    }
    return $factores;
}

// Crear una función para encontrar todos los divisores de un número.
function divisores($numero) {
    $divisores = [];
    for ($i = 1; $i <= $numero; $i++) {
        if ($numero % $i == 0) {
            $divisores[] = $i;
        }
    }
    return $divisores;
}

// Crear una función para encontrar el número perfecto más cercano a un número dado.
function numeroPerfectoMasCercano($numero) {
    $numeroPerfecto = 0;
    for ($i = $numero; $i >= 1; $i--) {
        if (sumaFactores($i) == $i) {
            $numeroPerfecto = $i;
            break;
        }
    }
    return $numeroPerfecto;
}

// Crear una función para encontrar la suma de los factores de un número.
function sumaFactores($numero) {
    $suma = 0;
    for ($i = 1; $i <= $numero / 2; $i++) {
        if ($numero % $i == 0) {
            $suma += $i;
        }
    }
    return $suma;
}

// Crear una función para encontrar el número primo más cercano a un número dado.
function numeroPrimoMasCercano($numero) {
    if ($numero <= 1) {
        return 2;
    } else {
        $numeroPrimo = 0;
        for ($i = $numero; $i >= 2; $i--) {
            if (esPrimo($i)) {
                $numeroPrimo = $i;
                break;
            }
        }
        return $numeroPrimo;
    }
}

// Crear una función para determinar si un número es primo.
function esPrimo($numero) {
    if ($numero <= 1) {
        return false;
    } elseif ($numero == 2) {
        return true;
    } else {
        for ($i = 2; $i <= sqrt($numero); $i++) {
            if ($numero % $i == 0) {
                return false;
            }
        }
        return true;
    }
}

// Crear una función para encontrar todos los números primos menores que un número dado.
function numerosPrimosMenoresQue($numero) {
    $numerosPrimos = [];
    for ($i = 2; $i < $numero; $i++) {
        if (esPrimo($i)) {
            $numerosPrimos[] = $i;
        }
    }
    return $numerosPrimos;
}

// Crear una función para encontrar el número de dígitos de un número.
function numeroDigitos($numero) {
    if ($numero < 0) {
        $numero = -