```php
<?php
// Creación de una función para calcular el factorial de un número.

function factorial($numero) {
  // Caso base: el factorial de 0 es 1.
  if ($numero == 0) {
    return 1;
  }

  // Recursión: el factorial de un número n es n * (n-1)!.
  else {
    return $numero * factorial($numero - 1);
  }
}

// Ejemplo de uso de la función factorial.
$numero = 5;
$resultado = factorial($numero);

// Impresión del resultado del factorial.
echo "El factorial de $numero es: $resultado";

// Creación de una función para generar una secuencia de Fibonacci.

function fibonacci($longitud) {
  // Caso base: la secuencia de Fibonacci de longitud 0 es [0].
  if ($longitud == 0) {
    return [0];
  }

  // Caso base: la secuencia de Fibonacci de longitud 1 es [0, 1].
  else if ($longitud == 1) {
    return [0, 1];
  }

  // Recursión: la secuencia de Fibonacci de longitud n es [F(n-1), F(n-2)] concatenada con F(n).
  else {
    $secuencia = fibonacci($longitud - 1);
    $ultimo_numero = $secuencia[$longitud - 2] + $secuencia[$longitud - 1];
    array_push($secuencia, $ultimo_numero);
    return $secuencia;
  }
}

// Ejemplo de uso de la función fibonacci.
$longitud = 10;
$resultado = fibonacci($longitud);

// Impresión del resultado de la secuencia de Fibonacci.
echo "\nLa secuencia de Fibonacci de longitud $longitud es: [";
foreach ($resultado as $numero) {
  echo "$numero, ";
}
echo ']';

// Creación de una función para determinar si un número es primo.

function es_primo($numero) {
  // Caso base: 1 no es primo.
  if ($numero == 1) {
    return false;
  }

  // Caso base: 2 es primo.
  else if ($numero == 2) {
    return true;
  }

  // Optimización: si el número es par, no es primo.
  else if ($numero % 2 == 0) {
    return false;
  }

  // Comprobación de la primalidad del número.
  else {
    for ($i = 3; $i <= sqrt($numero); $i += 2) {
      if ($numero % $i == 0) {
        return false;
      }
    }

    return true;
  }
}

// Ejemplo de uso de la función es_primo.
$numero = 13;
$resultado = es_primo($numero);

// Impresión del resultado de la comprobación de primalidad.
echo "\nEl número $numero es primo: " . ($resultado ? 'sí' : 'no');
```

Explicación del código:

1. Función factorial: Calcula el factorial de un número utilizando recursión.

2. Función fibonacci: Genera una secuencia de Fibonacci de una longitud especificada utilizando recursión.

3. Función es_primo: Determina si un número es primo o no utilizando una optimización para números pares y un bucle para comprobar los divisores del número.

Este código implementa funciones complejas y útiles en matemáticas y ciencias de la computación, como el cálculo del factorial, la generación de secuencias de Fibonacci y la comprobación de primalidad de números.