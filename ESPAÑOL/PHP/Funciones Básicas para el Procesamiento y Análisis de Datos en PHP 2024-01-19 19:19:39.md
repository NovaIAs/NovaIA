```php
<?php

// 1. Definir variables
$nombre = 'Juan';
$apellido = 'Pérez';
$edad = 25;
$genero = 'Masculino';
$hobbies = ['Leer', 'Viajar', 'Cocinar'];

// 2. Crear una función que salude a una persona
function saludar($nombre, $apellido) {
  echo "Hola $nombre $apellido!\n";
}

// 3. Crear una función que imprima los datos de una persona
function imprimirDatosPersonales($nombre, $apellido, $edad, $genero, $hobbies) {
  echo "Nombre: $nombre\n";
  echo "Apellido: $apellido\n";
  echo "Edad: $edad\n";
  echo "Género: $genero\n";
  echo "Hobbies:\n";
  foreach ($hobbies as $hobby) {
    echo "  * $hobby\n";
  }
}

// 4. Crear una función que calcule el IMC de una persona
function calcularIMC($peso, $altura) {
  $imc = $peso / ($altura * $altura);
  return $imc;
}

// 5. Crear una función que clasifique el IMC de una persona
function clasificarIMC($imc) {
  if ($imc < 18.5) {
    return 'Bajo peso';
  } elseif ($imc >= 18.5 && $imc < 25) {
    return 'Peso normal';
  } elseif ($imc >= 25 && $imc < 30) {
    return 'Sobrepeso';
  } else {
    return 'Obesidad';
  }
}

// 6. Crear una función que genere un número aleatorio entre dos números
function generarNumeroAleatorio($min, $max) {
  return rand($min, $max);
}

// 7. Crear una función que genere una cadena aleatoria de una longitud determinada
function generarCadenaAleatoria($longitud) {
  $cadena = '';
  for ($i = 0; $i < $longitud; $i++) {
    $caracter = chr(generarNumeroAleatorio(65, 90));
    $cadena .= $caracter;
  }
  return $cadena;
}

// 8. Crear una función que ordene un array de números de forma ascendente
function ordenarArrayAscendente($array) {
  sort($array);
  return $array;
}

// 9. Crear una función que ordene un array de números de forma descendente
function ordenarArrayDescendente($array) {
  rsort($array);
  return $array;
}

// 10. Crear una función que busque un elemento en un array
function buscarElementoEnArray($array, $elemento) {
  $indice = array_search($elemento, $array);
  return $indice;
}

// 11. Crear una función que elimine un elemento de un array
function eliminarElementoDeArray($array, $elemento) {
  $indice = array_search($elemento, $array);
  unset($array[$indice]);
  return $array;
}

// 12. Crear una función que añada un elemento a un array
function añadirElementoAArray($array, $elemento) {
  array_push($array, $elemento);
  return $array;
}

// 13. Crear una función que devuelva el valor máximo de un array
function valorMaximoArray($array) {
  return max($array);
}

// 14. Crear una función que devuelva el valor mínimo de un array
function valorMinimoArray($array) {
  return min($array);
}

// 15. Crear una función que devuelva la media de un array
function mediaArray($array) {
  return array_sum($array) / count($array);
}

// 16. Crear una función que devuelva la desviación estándar de un array
function desviacionEstandarArray($array) {
  $media = mediaArray($array);
  $varianzas = [];
  foreach ($array as $valor) {
    $varianza = pow($valor - $media, 2);
    array_push($varianzas, $varianza);
  }
  $varianza = array_sum($varianzas) / count($varianzas);
  $desviacionEstandar = sqrt($varianza);
  return $desviacionEstandar;
}

// 17. Crear una función que devuelva la moda de un array
function modaArray($array) {
  $moda = [];
  $frecuencias = [];
  foreach ($array as $valor) {
    if (!isset($frecuencias[$valor])) {
      $frecuencias[$valor] = 0;
    }
    $frecuencias[$valor]++;
  }
  $maxFrecuencia = max($frecuencias);
  foreach ($frecuencias as $valor => $frecuencia) {
    if ($frecuencia == $maxFrecuencia) {
      array_push($moda, $valor);
    }
  }
  return $moda;
}

// 18. Crear una función que devuelva la mediana de un array
function medianaArray($array) {
  sort($array);
  $longitud = count($array);
  if ($longitud % 2 == 0) {
    $indiceMedio1 = $longitud / 2 - 1;
    $indiceMedio2 = $longitud / 2;
    $mediana = ($array[$indiceMedio1] + $array[$indiceMedio2]) / 2;
  } else {
    $indiceMedio = ($longitud - 1) / 2;
    $mediana = $array[$indiceMedio];
  }
  return $mediana;
}

// 19. Crear una función que devuelva el rango de un array
function rangoArray($array) {
  sort($array);
  $rango = $array[count($array) - 1] - $array[0];
  return $rango;
}

// 20. Crear una función que devuelva la cuartil 1 de un array
function cuartil1Array($array) {
  sort($array);
  $