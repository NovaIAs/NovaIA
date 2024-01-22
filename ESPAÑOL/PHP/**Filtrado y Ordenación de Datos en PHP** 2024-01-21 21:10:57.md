```php
<?php

$datos = array(
    array(
        "nombre" => "Juan",
        "apellido" => "Pérez",
        "edad" => 25,
        "sexo" => "Masculino"
    ),
    array(
        "nombre" => "María",
        "apellido" => "García",
        "edad" => 30,
        "sexo" => "Femenino"
    ),
    array(
        "nombre" => "Pedro",
        "apellido" => "López",
        "edad" => 35,
        "sexo" => "Masculino"
    ),
    array(
        "nombre" => "Ana",
        "apellido" => "Fernández",
        "edad" => 40,
        "sexo" => "Femenino"
    ),
    array(
        "nombre" => "José",
        "apellido" => "Martínez",
        "edad" => 45,
        "sexo" => "Masculino"
    )
);

// Función para filtrar datos por edad
function filtrarPorEdad($dato, $edadMinima, $edadMaxima) {
    return ($dato["edad"] >= $edadMinima && $dato["edad"] <= $edadMaxima);
}

// Función para filtrar datos por sexo
function filtrarPorSexo($dato, $sexo) {
    return ($dato["sexo"] == $sexo);
}

// Función para ordenar datos por nombre
function ordenarPorNombre($a, $b) {
    return strcmp($a["nombre"], $b["nombre"]);
}

// Filtrar datos por edad entre 25 y 40 años
$datosFiltradosPorEdad = array_filter($datos, "filtrarPorEdad", 0, 25, 40);

// Filtrar datos por sexo masculino
$datosFiltradosPorSexo = array_filter($datos, "filtrarPorSexo", 0, "Masculino");

// Ordenar datos por nombre
usort($datos, "ordenarPorNombre");

// Imprimir resultados
echo "Datos filtrados por edad entre 25 y 40 años:<br>";
print_r($datosFiltradosPorEdad);

echo "<br>Datos filtrados por sexo masculino:<br>";
print_r($datosFiltradosPorSexo);

echo "<br>Datos ordenados por nombre:<br>";
print_r($datos);

?>
```

Explicación del código:

1. Definimos un array $datos que contiene información de varias personas, incluyendo su nombre, apellido, edad y sexo.
2. Definimos tres funciones:
    * `filtrarPorEdad`: Esta función filtra los datos por edad, devolviendo solo los datos de las personas cuya edad está entre una edad mínima y máxima especificadas.
    * `filtrarPorSexo`: Esta función filtra los datos por sexo, devolviendo solo los datos de las personas cuyo sexo coincide con el sexo especificado.
    * `ordenarPorNombre`: Esta función ordena los datos por nombre, utilizando la función incorporada `strcmp` para comparar los nombres.
3. Filtramos los datos por edad entre 25 y 40 años utilizando la función `array_filter` y la función `filtrarPorEdad`.
4. Filtramos los datos por sexo masculino utilizando la función `array_filter` y la función `filtrarPorSexo`.
5. Ordenamos los datos por nombre utilizando la función `usort` y la función `ordenarPorNombre`.
6. Finalmente, imprimimos los resultados en la pantalla utilizando la función `print_r`.