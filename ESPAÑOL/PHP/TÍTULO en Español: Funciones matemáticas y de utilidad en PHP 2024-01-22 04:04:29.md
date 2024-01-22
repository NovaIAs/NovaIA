```php
<?php

// Definir la función para calcular el factorial de un número
function factorial($n) {
  // Si el número es 0 o 1, retornar 1
  if ($n == 0 || $n == 1) {
    return 1;
  }
  // De lo contrario, llamar recursivamente a la función con el número menos 1
  else {
    return $n * factorial($n - 1);
  }
}

// Definir la función para encontrar el máximo común divisor de dos números
function gcd($a, $b) {
  // Si b es 0, retornar a
  if ($b == 0) {
    return $a;
  }
  // De lo contrario, llamar recursivamente a la función con b y el resto de a dividido por b
  else {
    return gcd($b, $a % $b);
  }
}

// Definir la función para encontrar el mínimo común múltiplo de dos números
function lcm($a, $b) {
  // Calcular el máximo común divisor de a y b
  $gcd = gcd($a, $b);
  // Retornar el producto de a y b dividido por el máximo común divisor
  return $a * $b / $gcd;
}

// Definir la función para generar una secuencia de Fibonacci de una longitud dada
function fibonacci($n) {
  // Si la longitud es 0 o 1, retornar una lista con 0 y 1, respectivamente
  if ($n == 0) {
    return [0];
  }
  else if ($n == 1) {
    return [0, 1];
  }
  // De lo contrario, llamar recursivamente a la función con la longitud menos 1 y añadir el último elemento de la secuencia resultante al final de la lista
  else {
    $sequence = fibonacci($n - 1);
    $last_element = $sequence[count($sequence) - 1];
    $second_last_element = $sequence[count($sequence) - 2];
    $sequence[] = $last_element + $second_last_element;
    return $sequence;
  }
}

// Definir la función para comprobar si un número es primo
function is_prime($n) {
  // Si el número es 1, retornar falso
  if ($n == 1) {
    return false;
  }
  // Si el número es par y mayor que 2, retornar falso
  if ($n % 2 == 0 && $n > 2) {
    return false;
  }
  // De lo contrario, iterar sobre los números impares hasta la raíz cuadrada del número y comprobar si alguno de ellos divide al número
  for ($i = 3; $i <= sqrt($n); $i += 2) {
    if ($n % $i == 0) {
      return false;
    }
  }
  // Si se llega al final del bucle sin encontrar un divisor, retornar verdadero
  return true;
}

// Definir la función para encontrar todos los números primos hasta un número dado
function prime_numbers($n) {
  // Crear una lista vacía para almacenar los números primos
  $prime_numbers = [];
  // Iterar sobre los números desde 2 hasta el número dado
  for ($i = 2; $i <= $n; $i++) {
    // Si el número es primo, añadirlo a la lista
    if (is_prime($i)) {
      $prime_numbers[] = $i;
    }
  }
  // Retornar la lista de números primos
  return $prime_numbers;
}

// Definir la función para encontrar el número más grande en una lista
function max_number($list) {
  // Establecer el número máximo inicial al primer elemento de la lista
  $max_number = $list[0];
  // Iterar sobre los elementos restantes de la lista
  for ($i = 1; $i < count($list); $i++) {
    // Si el elemento actual es mayor que el número máximo actual, actualizar el número máximo actual
    if ($list[$i] > $max_number) {
      $max_number = $list[$i];
    }
  }
  // Retornar el número máximo
  return $max_number;
}

// Definir la función para encontrar el número más pequeño en una lista
function min_number($list) {
  // Establecer el número mínimo inicial al primer elemento de la lista
  $min_number = $list[0];
  // Iterar sobre los elementos restantes de la lista
  for ($i = 1; $i < count($list); $i++) {
    // Si el elemento actual es menor que el número mínimo actual, actualizar el número mínimo actual
    if ($list[$i] < $min_number) {
      $min_number = $list[$i];
    }
  }
  // Retornar el número mínimo
  return $min_number;
}

// Definir la función para ordenar una lista en orden ascendente
function sort_ascending($list) {
  // Utilizar la función incorporada de PHP sort() para ordenar la lista en orden ascendente
  sort($list);
  // Retornar la lista ordenada
  return $list;
}

// Definir la función para ordenar una lista en orden descendente
function sort_descending