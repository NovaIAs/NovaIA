```php
<?php

// Definiciones de funciones

// Función para obtener el factorial de un número.
function factorial($numero) {
  if ($numero < 0) {
    throw new Exception("El factorial no está definido para números negativos.");
  }
  if ($numero == 0) {
    return 1;
  }
  return $numero * factorial($numero - 1);
}

// Función para determinar si un número es primo.
function esPrimo($numero) {
  if ($numero <= 1) {
    return false;
  }
  if ($numero <= 3) {
    return true;
  }
  if ($numero % 2 == 0 || $numero % 3 == 0) {
    return false;
  }
  for ($i = 5; $i * $i <= $numero; $i += 6) {
    if ($numero % $i == 0 || $numero % ($i + 2) == 0) {
      return false;
    }
  }
  return true;
}

// Función para obtener el máximo común divisor de dos números.
function maximoComunDivisor($numero1, $numero2) {
  while ($numero2 != 0) {
    $aux = $numero1;
    $numero1 = $numero2;
    $numero2 = $aux % $numero2;
  }
  return $numero1;
}

// Función para obtener el mínimo común múltiplo de dos números.
function minimoComunMultiplo($numero1, $numero2) {
  return ($numero1 * $numero2) / maximoComunDivisor($numero1, $numero2);
}

// Función para obtener la ecuación de segundo grado con raíces complejas.
function ecuacionSegundoGradoCompleja($a, $b, $c) {
  $discriminante = $b * $b - 4 * $a * $c;
  if ($discriminante < 0) {
    $parteReal = -$b / (2 * $a);
    $parteImaginaria = sqrt(-$discriminante) / (2 * $a);
    return "La ecuación tiene raíces complejas: " . $parteReal . " + " . $parteImaginaria . "i y " . $parteReal . " - " . $parteImaginaria . "i";
  } else {
    return "La ecuación no tiene raíces complejas.";
  }
}

// Programa principal

// Calcular el factorial de un número.
$numero = 5;
$factorial = factorial($numero);
echo "El factorial de $numero es $factorial" . PHP_EOL;

// Comprobar si un número es primo.
$numero = 13;
if (esPrimo($numero)) {
  echo "$numero es un número primo" . PHP_EOL;
} else {
  echo "$numero no es un número primo" . PHP_EOL;
}

// Calcular el máximo común divisor de dos números.
$numero1 = 12;
$numero2 = 18;
$mcd = maximoComunDivisor($numero1, $numero2);
echo "El máximo común divisor de $numero1 y $numero2 es $mcd" . PHP_EOL;

// Calcular el mínimo común múltiplo de dos números.
$numero1 = 10;
$numero2 = 15;
$mcm = minimoComunMultiplo($numero1, $numero2);
echo "El mínimo común múltiplo de $numero1 y $numero2 es $mcm" . PHP_EOL;

// Obtener la ecuación de segundo grado con raíces complejas.
$a = 1;
$b = 2;
$c = 5;
$ecuacion = ecuacionSegundoGradoCompleja($a, $b, $c);
echo $ecuacion . PHP_EOL;

?>
```

Explicación del código:

* La función `factorial` calcula el factorial de un número positivo.
* La función `esPrimo` determina si un número es primo.
* La función `maximoComunDivisor` obtiene el máximo común divisor de dos números.
* La función `minimoComunMultiplo` obtiene el mínimo común múltiplo de dos números.
* La función `ecuacionSegundoGradoCompleja` obtiene la ecuación de segundo grado con raíces complejas.

El programa principal utiliza las funciones definidas anteriormente para calcular el factorial de un número, comprobar si un número es primo, calcular el máximo común divisor y el mínimo común múltiplo de dos números, y obtener la ecuación de segundo grado con raíces complejas.