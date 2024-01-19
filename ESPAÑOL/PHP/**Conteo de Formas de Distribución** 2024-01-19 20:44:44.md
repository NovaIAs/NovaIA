```php
<?php

// Definir una función para calcular el factorial de un número
function factorial($número) {
  if ($número < 2) {
    return 1;
  } else {
    return $número * factorial($número - 1);
  }
}

// Definir una función para calcular el número de combinaciones de n elementos tomados de r en r
function combinaciones($n, $r) {
  return factorial($n) / (factorial($r) * factorial($n - $r));
}

// Definir una función para calcular el número de permutaciones de n elementos tomados de r en r
function permutaciones($n, $r) {
  return factorial($n) / factorial($n - $r);
}

// Definir una función para calcular el número de variaciones de n elementos tomados de r en r
function variaciones($n, $r) {
  return factorial($n) / factorial($n - $r);
}

// Definir una función para calcular el número de subconjuntos de n elementos tomados de r en r
function subconjuntos($n, $r) {
  return combinaciones($n, $r) + permutaciones($n, $r) + variaciones($n, $r);
}

// Definir una función para calcular el número de formas de distribuir n elementos en r contenedores, sin importar el orden
function distribuciones($n, $r) {
  return subconjuntos($n + $r - 1, $r - 1);
}

// Definir una función para calcular el número de formas de distribuir n elementos en r contenedores, con el orden impor
function arreglos($n, $r) {
  return permutaciones($n + $r - 1, $r - 1);
}

// Definir una función para calcular el número de formas de seleccionar r elementos de un conjunto de n elementos, sin importar el orden
function selecciones($n, $r) {
  return combinaciones($n, $r);
}

// Definir una función para calcular el número de formas de seleccionar r elementos de un conjunto de n elementos, con el orden impor
function permutaciones_simples($n, $r) {
  return permutaciones($n, $r);
}

// Definir una función para calcular el número de formas de seleccionar r elementos de un conjunto de n elementos, sin importar el orden y sin repeticiones
function variaciones_simples($n, $r) {
  return variaciones($n, $r);
}

// Definir una función para calcular el número de formas de seleccionar r elementos de un conjunto de n elementos, con el orden impor y sin repeticiones
function arreglos_simples($n, $r) {
  return arreglos($n, $r);
}

// Definir una función para calcular el número de formas de distribuir n elementos en r contenedores, sin importar el orden y sin repeticiones
function distribuciones_simples($n, $r) {
  return distribuciones($n, $r);
}

// Definir una función para imprimir una tabla de combinaciones, permutaciones, variaciones, subconjuntos, distribuciones, arreglos, selecciones, permutaciones simples, variaciones simples, arreglos simples y distribuciones simples
function imprimir_tabla($n, $r) {
  echo "<table>";
  echo "<tr><th>Tipo de conteo</th><th>Número de formas</th></tr>";
  echo "<tr><td>Combinaciones</td><td>" . combinaciones($n, $r) . "</td></tr>";
  echo "<tr><td>Permutaciones</td><td>" . permutaciones($n, $r) . "</td></tr>";
  echo "<tr><td>Variaciones</td><td>" . variaciones($n, $r) . "</td></tr>";
  echo "<tr><td>Subconjuntos</td><td>" . subconjuntos($n, $r) . "</td></tr>";
  echo "<tr><td>Distribuciones</td><td>" . distribuciones($n, $r) . "</td></tr>";
  echo "<tr><td>Arreglos</td><td>" . arreglos($n, $r) . "</td></tr>";
  echo "<tr><td>Selecciones</td><td>" . selecciones($n, $r) . "</td></tr>";
  echo "<tr><td>Permutaciones simples</td><td>" . permutaciones_simples($n, $r) . "</td></tr>";
  echo "<tr><td>Variaciones simples</td><td>" . variaciones_simples($n, $r) . "</td></tr>";
  echo "<tr><td>Arreglos simples</td><td>" . arreglos_simples($n, $r) . "</td></tr>";
  echo "<tr><td>Distribuciones simples</td><td>" . distribuciones_simples($n, $r) . "</td></tr>";
  echo "</table>";
}

// Obtener los valores de n y r de la entrada del usuario
$n = $_POST["n"];
$r = $_POST["r"];

// Imprimir la tabla de combinaciones, permutaciones, variaciones, subconjuntos, distribuciones, arreglos, selecciones, permutaciones simples, variaciones simples, arreglos simples y distribuciones simples
imprimir_tabla($n, $r);

?>
```

Este código es un programa PHP que calcula el número de formas de distribuir n elementos en r contenedores, con o sin importar el orden, y con o sin repeticiones. El programa también calcula el número de combinaciones, permutaciones, variaciones, subconjuntos, selecciones, permutaciones simples, variaciones simples, arreglos simples y distribuciones simples.

El programa define una serie de funciones para calcular el número de formas de distribuir n elementos en r contenedores, con o sin importar el orden, y con o sin repeticiones. Estas funciones se utilizan para calcular el número de combinaciones, permutaciones, variaciones, subconjuntos, selecciones, permutaciones simples, variaciones simples, arreglos simples y distribuciones simples.

El programa también define una función para imprimir una tabla de combinaciones, permutaciones, variaciones, subconjuntos, distribuciones, arreglos, selecciones, permutaciones simples, variaciones simples, arreglos simples y distribuciones simples. Esta función se utiliza para imprimir la tabla de resultados.

El programa obtiene los valores de n y r de la entrada del usuario. El usuario puede introducir cualquier valor de n y r.

El programa imprime la tabla de combinaciones, permutaciones, variaciones, subconjuntos, distribuciones, arreglos, selecciones, permutaciones simples, variaciones simples, arreglos simples y distribuciones simples. La tabla muestra el número de formas de distribuir n elementos en r contenedores, con o sin importar el orden, y con o sin repeticiones.