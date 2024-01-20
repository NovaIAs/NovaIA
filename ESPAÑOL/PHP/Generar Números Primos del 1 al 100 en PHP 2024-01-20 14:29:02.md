```php
// Definimos las constantes que utilizaremos en el código
const NUMERO_MAXIMO = 100;
const NUMERO_MINIMO = 1;

// Creamos un array con los números del 1 al 100
$numeros = range(NUMERO_MINIMO, NUMERO_MAXIMO);

// Creamos un array vacío para almacenar los números primos
$numerosPrimos = array();

// Recorremos el array de números del 1 al 100
foreach ($numeros as $numero) {
  // Si el número es primo, lo añadimos al array de números primos
  if (esPrimo($numero)) {
    $numerosPrimos[] = $numero;
  }
}

// Imprimimos el array de números primos
print_r($numerosPrimos);

// Función para comprobar si un número es primo
function esPrimo($numero) {
  // Si el número es 1, no es primo
  if ($numero == 1) {
    return false;
  }

  // Comprobamos si el número es divisible por algún número del 2 al número - 1
  for ($i = 2; $i < $numero; $i++) {
    if ($numero % $i == 0) {
      return false;
    }
  }

  // Si el número no es divisible por ningún número del 2 al número - 1, es primo
  return true;
}
```

Este código genera un array con los números primos del 1 al 100.

El código empieza definiendo las constantes NUMERO_MAXIMO y NUMERO_MINIMO, que se utilizan para generar el array de números del 1 al 100.

Luego, se crea un array vacío para almacenar los números primos.

A continuación, se recorre el array de números del 1 al 100 y se comprueba si cada número es primo. Si lo es, se añade al array de números primos.

Finalmente, se imprime el array de números primos.

La función esPrimo() se utiliza para comprobar si un número es primo. La función devuelve true si el número es primo y false en caso contrario.

La función esPrimo() comprueba primero si el número es 1. Si lo es, devuelve false porque los números primos son mayores que 1.

A continuación, la función recorre los números del 2 al número - 1 y comprueba si el número es divisible por alguno de estos números. Si lo es, devuelve false porque un número primo no es divisible por ningún otro número excepto por sí mismo y por 1.

Si la función no encuentra ningún número del 2 al número - 1 que sea divisor del número, devuelve true porque el número es primo.