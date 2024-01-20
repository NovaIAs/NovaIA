```php
<?php

// Función para generar un número aleatorio entre dos valores
function generarNumeroAleatorio($min, $max) {
  return rand($min, $max);
}

// Función para generar una cadena aleatoria de una longitud determinada
function generarCadenaAleatoria($longitud) {
  $caracteres = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  $cadena = '';
  for ($i = 0; $i < $longitud; $i++) {
    $cadena .= $caracteres[generarNumeroAleatorio(0, strlen($caracteres) - 1)];
  }
  return $cadena;
}

// Función para crear un array de objetos con propiedades aleatorias
function crearArrayDeObjetos($longitud) {
  $array = [];
  for ($i = 0; $i < $longitud; $i++) {
    $objeto = new stdClass();
    $objeto->nombre = generarCadenaAleatoria(10);
    $objeto->edad = generarNumeroAleatorio(18, 65);
    $objeto->ocupacion = generarCadenaAleatoria(15);
    $array[] = $objeto;
  }
  return $array;
}

// Función para imprimir un array de objetos en formato JSON
function imprimirArrayDeObjetosJSON($array) {
  header('Content-Type: application/json');
  echo json_encode($array);
}

// Crear un array de objetos con 10 elementos
$arrayDeObjetos = crearArrayDeObjetos(10);

// Imprimir el array de objetos en formato JSON
imprimirArrayDeObjetosJSON($arrayDeObjetos);

?>
```

Explicación del código:

* La función `generarNumeroAleatorio()` genera un número aleatorio entre dos valores dados.
* La función `generarCadenaAleatoria()` genera una cadena aleatoria de una longitud determinada.
* La función `crearArrayDeObjetos()` crea un array de objetos con propiedades aleatorias.
* La función `imprimirArrayDeObjetosJSON()` imprime un array de objetos en formato JSON.

El código utiliza estas funciones para generar un array de 10 objetos con propiedades aleatorias y luego imprime el array en formato JSON.