```php
// Función para generar números aleatorios
function generarNumeroAleatorio($min, $max) {
  return rand($min, $max);
}

// Función para generar una cadena aleatoria
function generarCadenaAleatoria($longitud) {
  $cadena = "";
  for ($i = 0; $i < $longitud; $i++) {
    $cadena .= chr(generarNumeroAleatorio(65, 90));
  }
  return $cadena;
}

// Función para generar un array aleatorio
function generarArrayAleatorio($longitud) {
  $array = [];
  for ($i = 0; $i < $longitud; $i++) {
    $array[] = generarNumeroAleatorio(1, 100);
  }
  return $array;
}

// Función para imprimir un array
function imprimirArray($array) {
  echo "<pre>";
  print_r($array);
  echo "</pre>";
}

// Función para buscar un elemento en un array
function buscarElementoEnArray($array, $elemento) {
  $indice = array_search($elemento, $array);
  return $indice;
}

// Función para ordenar un array de menor a mayor
function ordenarArray MenorAMayor($array) {
  sort($array);
  return $array;
}

// Función para ordenar un array de mayor a menor
function ordenarArrayMayorAMenor($array) {
  rsort($array);
  return $array;
}

// Función para invertir un array
function invertirArray($array) {
  $arrayInvertido = array_reverse($array);
  return $arrayInvertido;
}

// Función para unir dos arrays
function unirArrays($array1, $array2) {
  $arrayUnido = array_merge($array1, $array2);
  return $arrayUnido;
}

// Función para eliminar un elemento de un array
function eliminarElementoDeArray($array, $elemento) {
  $indice = array_search($elemento, $array);
  if ($indice !== false) {
    unset($array[$indice]);
  }
  return $array;
}

// Función para filtrar un array
function filtrarArray($array, $funcionFiltro) {
  $arrayFiltrado = array_filter($array, $funcionFiltro);
  return $arrayFiltrado;
}

// Función para reducir un array a un único valor
function reducirArray($array, $funcionReductora, $valorInicial) {
  $valorReducido = array_reduce($array, $funcionReductora, $valorInicial);
  return $valorReducido;
}

// Función para generar un objeto aleatorio
function generarObjetoAleatorio() {
  $objeto = new stdClass();
  $objeto->nombre = generarCadenaAleatoria(10);
  $objeto->edad = generarNumeroAleatorio(1, 100);
  $objeto->sexo = generarNumeroAleatorio(0, 1) ? "masculino" : "femenino";
  return $objeto;
}

// Función para imprimir un objeto
function imprimirObjeto($objeto) {
  echo "<pre>";
  print_r($objeto);
  echo "</pre>";
}

// Función para buscar una propiedad en un objeto
function buscarPropiedadEnObjeto($objeto, $propiedad) {
  if (property_exists($objeto, $propiedad)) {
    return $objeto->$propiedad;
  } else {
    return null;
  }
}

// Función para ordenar un array de objetos por una propiedad
function ordenarArrayDeObjetosPorPropiedad($array, $propiedad) {
  usort($array, function ($a, $b) use ($propiedad) {
    return strcmp($a->$propiedad, $b->$propiedad);
  });
  return $array;
}

// Función para filtrar un array de objetos por una propiedad
function filtrarArrayDeObjetosPorPropiedad($array, $propiedad, $valor) {
  $arrayFiltrado = array_filter($array, function ($objeto) use ($propiedad, $valor) {
    return $objeto->$propiedad == $valor;
  });
  return $arrayFiltrado;
}

// Función principal
function main() {
  // Generar un número aleatorio
  $numeroAleatorio = generarNumeroAleatorio(1, 100);
  echo "Número aleatorio: $numeroAleatorio\n";

  // Generar una cadena aleatoria
  $cadenaAleatoria = generarCadenaAleatoria(10);
  echo "Cadena aleatoria: $cadenaAleatoria\n";

  // Generar un array aleatorio
  $arrayAleatorio = generarArrayAleatorio(10);
  echo "Array aleatorio:\n";
  imprimirArray($arrayAleatorio);

  // Buscar un elemento en un array
  $elementoABuscar = 50;
  $indiceElemento = buscarElementoEnArray($arrayAleatorio, $elementoABuscar);
  echo "Índice del elemento $elementoABuscar en el array: $indiceElemento\n";

  // Ordenar un array de menor a mayor
  $arrayOrdenadoMenorAMayor = ordenarArray MenorAMayor($array