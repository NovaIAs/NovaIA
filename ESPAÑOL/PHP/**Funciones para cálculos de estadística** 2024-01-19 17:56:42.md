```php
<?php

// Función que calcula el factorial de un número
function factorial($n) {
  if ($n == 0) {
    return 1;
  } else {
    return $n * factorial($n - 1);
  }
}

// Función que calcula la combinación de dos números
function combinacion($n, $r) {
  if ($r > $n) {
    return 0;
  } else {
    return factorial($n) / (factorial($r) * factorial($n - $r));
  }
}

// Función que calcula la permutación de dos números
function permutacion($n, $r) {
  if ($r > $n) {
    return 0;
  } else {
    return factorial($n) / factorial($n - $r);
  }
}

// Función que calcula la probabilidad de un evento
function probabilidad($p, $n, $m) {
  return combinacion($n, $m) * pow($p, $m) * pow(1 - $p, $n - $m);
}

// Función que calcula la distribución binomial
function binomial($n, $p, $m) {
  return probabilidad($p, $n, $m) * factorial($n) / (factorial($m) * factorial($n - $m));
}

// Función que calcula la media de una distribución binomial
function media_binomial($n, $p) {
  return $n * $p;
}

// Función que calcula la varianza de una distribución binomial
function varianza_binomial($n, $p) {
  return $n * $p * (1 - $p);
}

// Función que calcula la desviación estándar de una distribución binomial
function desviacion_estandar_binomial($n, $p) {
  return sqrt(varianza_binomial($n, $p));
}

// Función que calcula la probabilidad de un evento en una distribución binomial
function probabilidad_binomial($n, $p, $m) {
  return binomial($n, $p, $m) * pow($p, $m) * pow(1 - $p, $n - $m);
}

// Función que calcula la probabilidad acumulada de un evento en una distribución binomial
function probabilidad_acumulada_binomial($n, $p, $m) {
  $sum = 0;
  for ($i = 0; $i <= $m; $i++) {
    $sum += probabilidad_binomial($n, $p, $i);
  }
  return $sum;
}

// Función que calcula la mediana de una distribución binomial
function mediana_binomial($n, $p) {
  $m = floor($n * $p);
  if ($m % 2 == 0) {
    return ($m + $m + 1) / 2;
  } else {
    return $m + 1;
  }
}

// Función que calcula la moda de una distribución binomial
function moda_binomial($n, $p) {
  $m = floor($n * $p);
  if ($m < 0) {
    return 0;
  } else if ($m > $n) {
    return $n;
  } else {
    return $m;
  }
}

// Ejemplo de uso de las funciones

// Calcular el factorial de 5
echo "Factorial de 5: " . factorial(5) . "\n";

// Calcular la combinación de 10 y 5
echo "Combinación de 10 y 5: " . combinacion(10, 5) . "\n";

// Calcular la permutación de 10 y 5
echo "Permutación de 10 y 5: " . permutacion(10, 5) . "\n";

// Calcular la probabilidad de obtener 3 caras al lanzar una moneda 5 veces
echo "Probabilidad de obtener 3 caras al lanzar una moneda 5 veces: " . probabilidad(0.5, 5, 3) . "\n";

// Calcular la distribución binomial para n = 10, p = 0.5 y m = 5
echo "Distribución binomial para n = 10, p = 0.5 y m = 5: \n";
for ($i = 0; $i <= 10; $i++) {
  echo "Probabilidad de obtener $i éxitos: " . binomial(10, 0.5, $i) . "\n";
}

// Calcular la media de una distribución binomial para n = 10 y p = 0.5
echo "Media de una distribución binomial para n = 10 y p = 0.5: " . media_binomial(10, 0.5) . "\n";

// Calcular la varianza de una distribución binomial para n = 10 y p = 0.5
echo "Varianza de una distribución binomial para n = 10 y p = 0.5: " . varianza_binomial(10, 0.5) . "\n";

// Calcular la desviación estándar de una distribución binomial para n = 10 y p = 0.5
echo "Desviación estándar de una distribución binomial para n = 10 y p = 0.5: " . desviacion_estandar_binomial(10, 0.5) . "\n";

// Calcular la probabilidad de obtener 3 caras al lanzar una moneda 5 veces
echo "Probabilidad de obtener 3 caras al lanzar una moneda 5 veces: " . probabilidad_binomial(10, 0.5, 3) . "\n";

// Calcular la probabilidad acumulada de obtener 3 caras al lanzar una moneda 5 veces
echo "Probabilidad acumulada de obtener 3 caras al lanzar una moneda 5 veces: " . probabilidad_acumulada_binomial(10, 0.5, 3) . "\n";

// Calcular la mediana de una distribución binomial para n = 10 y p = 0.5
echo "Mediana de una distribución binomial para n = 10 y p = 0.5: " . mediana_binomial(10, 0.5) . "\n";

// Calcular la moda de una distribución binomial para n = 10 y p = 0.5
echo "Moda de una distribución binomial para n = 10 y p = 0.5: " . moda_binomial(10, 0.5) . "\n";

?>
```

Explicación del código:

* La función `factorial()` calcula el factorial de un número dado.
* La función `combinacion()` calcula la combinación de dos números dados.
* La función `permutacion()` calcula la permutación de dos números dados.
* La función `probabilidad()` calcula la probabilidad de un evento dado.
* La función `binomial()` calcula la distribución binomial para un número dado de ensayos, una probabilidad de éxito y un número de éxitos.
* La función `media_binomial()` calcula la media de una distribución binomial.
* La función `varianza_binomial()` calcula la varianza de una distribución binomial.
* La función `desviacion_estandar_binomial()` calcula la desviación estándar de una distribución binomial.
* La función `probabilidad_binomial()` calcula la probabilidad de un evento dado en una distribución binomial.
* La función `probabilidad_acumulada_binomial()` calcula la probabilidad acumulada de un evento dado en una distribución binomial.
* La función `mediana_binomial()` calcula la mediana de una distribución binomial.
* La función `moda_binomial()` calcula la moda de una distribución binomial.