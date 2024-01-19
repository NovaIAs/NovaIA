```php
<?php

// Función para generar un número aleatorio entre dos números
function generarNumeroAleatorio($minimo, $maximo) {
    return rand($minimo, $maximo);
}

// Función para verificar si un número es primo
function esPrimo($numero) {
    if ($numero <= 1) {
        return false;
    }
    for ($i = 2; $i <= sqrt($numero); $i++) {
        if ($numero % $i == 0) {
            return false;
        }
    }
    return true;
}

// Función para generar una lista de números primos entre dos números
function generarListaPrimos($minimo, $maximo) {
    $listaPrimos = array();
    for ($i = $minimo; $i <= $maximo; $i++) {
        if (esPrimo($i)) {
            $listaPrimos[] = $i;
        }
    }
    return $listaPrimos;
}

// Función para calcular el factorial de un número
function factorial($numero) {
    if ($numero == 0) {
        return 1;
    } else {
        return $numero * factorial($numero - 1);
    }
}

// Función para calcular la combinación de dos números
function combinacion($n, $k) {
    return factorial($n) / (factorial($k) * factorial($n - $k));
}

// Función para calcular la permutación de dos números
function permutacion($n, $k) {
    return factorial($n) / factorial($n - $k);
}

// Función para calcular el máximo común divisor de dos números
function maximoComunDivisor($a, $b) {
    if ($b == 0) {
        return $a;
    } else {
        return maximoComunDivisor($b, $a % $b);
    }
}

// Función para calcular el mínimo común múltiplo de dos números
function minimoComunMultiplo($a, $b) {
    return ($a * $b) / maximoComunDivisor($a, $b);
}

// Función para calcular la raíz cuadrada de un número
function raizCuadrada($numero) {
    return sqrt($numero);
}

// Función para calcular la potencia de un número
function potencia($base, $exponente) {
    return pow($base, $exponente);
}

// Función para calcular el logaritmo de un número
function logaritmo($numero, $base) {
    return log($numero, $base);
}

// Función para calcular el seno de un ángulo
function seno($angulo) {
    return sin($angulo);
}

// Función para calcular el coseno de un ángulo
function coseno($angulo) {
    return cos($angulo);
}

// Función para calcular la tangente de un ángulo
function tangente($angulo) {
    return tan($angulo);
}

// Función para calcular el arco seno de un número
function arcoSeno($numero) {
    return asin($numero);
}

// Función para calcular el arco coseno de un número
function arcoCoseno($numero) {
    return acos($numero);
}

// Función para calcular el arco tangente de un número
function arcoTangente($numero) {
    return atan($numero);
}

// Función para calcular la hipotenusa de un triángulo rectángulo
function hipotenusa($catetoA, $catetoB) {
    return sqrt($catetoA * $catetoA + $catetoB * $catetoB);
}

// Función para calcular el área de un triángulo rectángulo
function areaTrianguloRectangulo($catetoA, $catetoB) {
    return 0.5 * $catetoA * $catetoB;
}

// Función para calcular el perímetro de un triángulo rectángulo
function perimetroTrianguloRectangulo($catetoA, $catetoB) {
    return $catetoA + $catetoB + hipotenusa($catetoA, $catetoB);
}

```

Este código proporciona una variedad de funciones matemáticas comunes, incluyendo:

* Generación de números aleatorios
* Comprobación de primalidad
* Generación de listas de números primos
* Cálculo de factoriales
* Cálculo de combinaciones y permutaciones
* Cálculo del máximo común divisor y el mínimo común múltiplo
* Cálculo de raíces cuadradas, potencias y logaritmos
* Cálculo de funciones trigonométricas
* Cálculo de la hipotenusa, área y perímetro de un triángulo rectángulo

Este código puede ser útil para una variedad de aplicaciones, incluyendo:

* Desarrollo de juegos
* Simulación matemática
* Gráficos por ordenador
* Procesamiento de señales
* Análisis de datos