```php
<?php
// Función que calcula el factorial de un número
function factorial($numero) {
    if ($numero == 0) {
        return 1;
    } else {
        return $numero * factorial($numero - 1);
    }
}

// Función que calcula la potencia de un número
function potencia($base, $exponente) {
    if ($exponente == 0) {
        return 1;
    } else {
        return $base * potencia($base, $exponente - 1);
    }
}

// Función que calcula el máximo común divisor de dos números
function mcd($numero1, $numero2) {
    if ($numero2 == 0) {
        return abs($numero1);
    } else {
        return mcd($numero2, $numero1 % $numero2);
    }
}

// Función que calcula el mínimo común múltiplo de dos números
function mcm($numero1, $numero2) {
    return abs($numero1 * $numero2) / mcd($numero1, $numero2);
}

// Función que calcula la raíz cuadrada de un número
function raiz_cuadrada($numero) {
    if ($numero < 0) {
        return "No se puede calcular la raíz cuadrada de un número negativo";
    } else {
        return sqrt($numero);
    }
}

// Función que calcula el logaritmo de un número
function logaritmo($numero, $base) {
    if ($numero <= 0 || $base <= 0 || $base == 1) {
        return "No se puede calcular el logaritmo de un número o una base no positivos o de una base igual a 1";
    } else {
        return log($numero, $base);
    }
}

// Función que calcula el seno de un ángulo
function seno($angulo) {
    return sin($angulo);
}

// Función que calcula el coseno de un ángulo
function coseno($angulo) {
    return cos($angulo);
}

// Función que calcula la tangente de un ángulo
function tangente($angulo) {
    return tan($angulo);
}

// Función que calcula el arco seno de un número
function arco_seno($numero) {
    if ($numero < -1 || $numero > 1) {
        return "No se puede calcular el arco seno de un número fuera del rango [-1, 1]";
    } else {
        return asin($numero);
    }
}

// Función que calcula el arco coseno de un número
function arco_coseno($numero) {
    if ($numero < -1 || $numero > 1) {
        return "No se puede calcular el arco coseno de un número fuera del rango [-1, 1]";
    } else {
        return acos($numero);
    }
}

// Función que calcula el arco tangente de un número
function arco_tangente($numero) {
    return atan($numero);
}

// Función que calcula la exponencial de un número
function exponencial($numero) {
    return exp($numero);
}

// Función que calcula el logaritmo natural de un número
function logaritmo_natural($numero) {
    if ($numero <= 0) {
        return "No se puede calcular el logaritmo natural de un número no positivo";
    } else {
        return log($numero);
    }
}

// Función que calcula la distancia entre dos puntos en un plano cartesiano
function distancia($x1, $y1, $x2, $y2) {
    return sqrt(pow($x2 - $x1, 2) + pow($y2 - $y1, 2));
}

// Función que calcula el área de un triángulo
function area_triangulo($base, $altura) {
    return 0.5 * $base * $altura;
}

// Función que calcula el área de un círculo
function area_circulo($radio) {
    return pi() * pow($radio, 2);
}

// Función que calcula el volumen de una esfera
function volumen_esfera($radio) {
    return (4/3) * pi() * pow($radio, 3);
}

// Función que calcula el volumen de un cono
function volumen_cono($radio, $altura) {
    return (1/3) * pi() * pow($radio, 2) * $altura;
}

// Función que calcula el volumen de un cilindro
function volumen_cilindro($radio, $altura) {
    return pi() * pow($radio, 2) * $altura;
}

// Función que calcula el volumen de un cubo
function volumen_cubo($lado) {
    return pow($lado, 3);
}

// Función que calcula el volumen de un paralelepípedo
function volumen_paralelepipedo($largo, $ancho, $alto) {
    return $largo * $ancho * $alto;
}

// Función que calcula el volumen de una pirámide
function volumen_piram