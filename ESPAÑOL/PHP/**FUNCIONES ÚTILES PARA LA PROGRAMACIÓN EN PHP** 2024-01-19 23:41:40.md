```php
<?php

// Definir una función para calcular el factorial de un número.
function factorial($n) {
  if ($n == 0) {
    return 1;
  } else {
    return $n * factorial($n - 1);
  }
}

// Definir una función para generar números aleatorios.
function generar_aleatorio($min, $max) {
  return rand($min, $max);
}

// Definir una función para generar una cadena aleatoria.
function generar_cadena($longitud) {
  $cadena = "";
  for ($i = 0; $i < $longitud; $i++) {
    $cadena .= chr(generar_aleatorio(65, 90));
  }
  return $cadena;
}

// Definir una función para ordenar un array.
function ordenar($array) {
  sort($array);
  return $array;
}

// Definir una función para buscar un elemento en un array.
function buscar($array, $elemento) {
  return array_search($elemento, $array);
}

// Definir una función para eliminar un elemento de un array.
function eliminar($array, $elemento) {
  $indice = buscar($array, $elemento);
  if ($indice !== false) {
    unset($array[$indice]);
  }
  return $array;
}

// Definir una función para añadir un elemento a un array.
function añadir($array, $elemento) {
  $array[] = $elemento;
  return $array;
}

// Definir una función para invertir una cadena.
function invertir($cadena) {
  return strrev($cadena);
}

// Definir una función para comprobar si una cadena es un palíndromo.
function es_palindromo($cadena) {
  return $cadena == invertir($cadena);
}

// Definir una función para generar una matriz.
function generar_matriz($filas, $columnas) {
  $matriz = array();
  for ($i = 0; $i < $filas; $i++) {
    for ($j = 0; $j < $columnas; $j++) {
      $matriz[$i][$j] = generar_aleatorio(0, 9);
    }
  }
  return $matriz;
}

// Definir una función para mostrar una matriz.
function mostrar_matriz($matriz) {
  echo "<pre>";
  print_r($matriz);
  echo "</pre>";
}

// Definir una función para transponer una matriz.
function transponer($matriz) {
  $matriz_transpuesta = array();
  for ($i = 0; $i < count($matriz); $i++) {
    for ($j = 0; $j < count($matriz[0]); $j++) {
      $matriz_transpuesta[$j][$i] = $matriz[$i][$j];
    }
  }
  return $matriz_transpuesta;
}

// Definir una función para calcular la diagonal principal de una matriz.
function diagonal_principal($matriz) {
  $diagonal = array();
  for ($i = 0; $i < count($matriz); $i++) {
    $diagonal[] = $matriz[$i][$i];
  }
  return $diagonal;
}

// Definir una función para calcular la diagonal secundaria de una matriz.
function diagonal_secundaria($matriz) {
  $diagonal = array();
  for ($i = 0; $i < count($matriz); $i++) {
    $diagonal[] = $matriz[$i][count($matriz) - $i - 1];
  }
  return $diagonal;
}

// Definir una función para calcular la suma de los elementos de una matriz.
function suma_matriz($matriz) {
  $