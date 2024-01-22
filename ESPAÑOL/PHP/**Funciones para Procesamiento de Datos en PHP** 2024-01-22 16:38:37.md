```php
<?php

// Definir una función que tome un array de enteros y devuelva el promedio.
function promedio($numeros) {
  // Sumar todos los números del array.
  $suma = 0;
  foreach ($numeros as $numero) {
    $suma += $numero;
  }

  // Dividir la suma por el número de elementos del array.
  $promedio = $suma / count($numeros);

  // Devolver el promedio.
  return $promedio;
}

// Definir una función que tome un array de strings y devuelva el string más largo.
function cadena_mas_larga($cadenas) {
  // Inicializar la variable para la cadena más larga con una cadena vacía.
  $cadena_mas_larga = "";

  // Recorrer el array de cadenas.
  foreach ($cadenas as $cadena) {
    // Si la cadena actual es más larga que la cadena más larga anterior, actualizar la variable.
    if (strlen($cadena) > strlen($cadena_mas_larga)) {
      $cadena_mas_larga = $cadena;
    }
  }

  // Devolver la cadena más larga.
  return $cadena_mas_larga;
}

// Definir una función que tome un array de objetos y devuelva el objeto con el valor más alto de una propiedad dada.
function objeto_con_valor_mas_alto($objetos, $propiedad) {
  // Inicializar la variable para el objeto con el valor más alto con el primer objeto del array.
  $objeto_con_valor_mas_alto = $objetos[0];

  // Recorrer el array de objetos.
  foreach ($objetos as $objeto) {
    // Si el valor de la propiedad dada del objeto actual es mayor que el valor de la propiedad del objeto con el valor más alto anterior, actualizar la variable.
    if ($objeto->$propiedad > $objeto_con_valor_mas_alto->$propiedad) {
      $objeto_con_valor_mas_alto = $objeto;
    }
  }

  // Devolver el objeto con el valor más alto.
  return $objeto_con_valor_mas_alto;
}

// Ejemplo de uso de las funciones.

// Crear un array de enteros.
$numeros = [1, 2, 3, 4, 5];

// Obtener el promedio del array de enteros.
$promedio_numeros = promedio($numeros);

// Crear un array de strings.
$cadenas = ["Hola", "Mundo", "PHP"];

// Obtener el string más largo del array de strings.
$cadena_mas_larga = cadena_mas_larga($cadenas);

// Crear un array de objetos.
$objetos = [
  (object) ["nombre" => "Juan", "edad" => 20],
  (object) ["nombre" => "María", "edad" => 25],
  (object) ["nombre" => "Pedro", "edad" => 30]
];

// Obtener el objeto con la edad más alta del array de objetos.
$objeto_con_edad_mas_alta = objeto_con_valor_mas_alto($objetos, "edad");

// Mostrar los resultados.
echo "El promedio de los números es: $promedio_numeros\n";
echo "El string más largo es: $cadena_mas_larga\n";
echo "El objeto con la edad más alta es: $objeto_con_edad_mas_alta->nombre, $objeto_con_edad_mas_alta->edad años\n";

?>
```

Explicación:

* La función `promedio()` toma un array de enteros y devuelve el promedio.
* La función `cadena_mas_larga()` toma un array de strings y devuelve el string más largo.
* La función `objeto_con_valor_mas_alto()` toma un array de objetos y una propiedad, y devuelve el objeto con el valor más alto de esa propiedad.

El código de ejemplo crea arrays de enteros, strings y objetos, y utiliza las funciones para obtener el promedio, el string más largo y el objeto con la edad más alta, respectivamente.