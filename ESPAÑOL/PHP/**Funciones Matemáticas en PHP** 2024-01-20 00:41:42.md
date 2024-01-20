```php
<?php

// Función para encontrar el factorial de un número
function factorial($n) {
  if ($n == 0) {
    return 1;
  } else {
    return $n * factorial($n-1);
  }
}

// Función para encontrar el máximo común divisor de dos números
function mcd($a, $b) {
  if ($b == 0) {
    return $a;
  } else {
    return mcd($b, $a % $b);
  }
}

// Función para encontrar el mínimo común múltiplo de dos números
function mcm($a, $b) {
  return ($a * $b) / mcd($a, $b);
}

// Función para generar una lista de números primos
function primos($n) {
  $primos = [];
  for ($i = 2; $i <= $n; $i++) {
    $esPrimo = true;
    for ($j = 2; $j <= $i/2; $j++) {
      if ($i % $j == 0) {
        $esPrimo = false;
        break;
      }
    }
    if ($esPrimo) {
      $primos[] = $i;
    }
  }
  return $primos;
}

// Función para generar una lista de números perfectos
function perfectos($n) {
  $perfectos = [];
  for ($i = 2; $i <= $n; $i++) {
    $divisores = [];
    for ($j = 1; $j <= $i/2; $j++) {
      if ($i % $j == 0) {
        $divisores[] = $j;
      }
    }
    $suma = array_sum($divisores);
    if ($suma == $i) {
      $perfectos[] = $i;
    }
  }
  return $perfectos;
}

// Función para generar una lista de números de Fibonacci
function fibonacci($n) {
  $fibonacci = [];
  $fibonacci[0] = 0;
  $fibonacci[1] = 1;
  for ($i = 2; $i <= $n; $i++) {
    $fibonacci[$i] = $fibonacci[$i-1] + $fibonacci[$i-2];
  }
  return $fibonacci;
}

// Función para generar una lista de números de Catalan
function catalan($n) {
  $catalan = [];
  $catalan[0] = 1;
  for ($i = 1; $i <= $n; $i++) {
    $catalan[$i] = 0;
    for ($j = 0; $j < $i; $j++) {
      $catalan[$i] += $catalan[$j] * $catalan[$i-$j-1];
    }
  }
  return $catalan;
}

// Función para generar una lista de números de Stirling del primer tipo
function stirling1($n) {
  $stirling1 = [];
  for ($i = 0; $i <= $n; $i++) {
    $stirling1[$i] = [];
    $stirling1[$i][0] = 0;
    $stirling1[$i][1] = 1;
    for ($j = 2; $j <= $i; $j++) {
      $stirling1[$i][$j] = ($j * $stirling1[$i-1][$j]) + $stirling1[$i-1][$j-1];
    }
  }
  return $stirling1;
}

// Función para generar una lista de números de Stirling del segundo tipo
function stirling2($n) {
  $stirling2 = [];
  for ($i = 0; $i <= $n; $i++) {
    $stirling2[$i] = [];
    $stirling2[$i][0] = 0;
    $stirling2[$i][1] = 1;
    for ($j = 2; $j <= $i; $j++) {
      $stirling2[$i][$j] = ($j * $stirling2[$i-1][$j]) - $stirling