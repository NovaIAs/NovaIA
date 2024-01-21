```php
<?php

// Definir las funciones necesarias
function obtener_datos_de_la_base_de_datos($consulta) {
  // Conectar a la base de datos
  $conexion = new mysqli("localhost", "usuario", "contraseña", "base_de_datos");

  // Ejecutar la consulta
  $resultado = $conexion->query($consulta);

  // Obtener los datos de la consulta
  $datos = array();
  while ($fila = $resultado->fetch_assoc()) {
    $datos[] = $fila;
  }

  // Cerrar la conexión a la base de datos
  $conexion->close();

  // Devolver los datos
  return $datos;
}

function procesar_datos($datos) {
  // Procesar los datos de la forma deseada
  foreach ($datos as &$fila) {
    // Obtener el valor de una columna específica
    $valor = $fila['columna_especifica'];

    // Procesar el valor de la columna
    $fila['columna_especifica'] = strtoupper($valor);
  }

  // Devolver los datos procesados
  return $datos;
}

function generar_informe($datos) {
  // Crear un objeto de informe
  $informe = new Informe();

  // Añadir los datos al informe
  foreach ($datos as $fila) {
    $informe->addFila($fila);
  }

  // Generar el informe en formato HTML
  $html = $informe->generarHTML();

  // Devolver el informe en formato HTML
  return $html;
}

// Obtener los datos de la base de datos
$datos = obtener_datos_de_la_base_de_datos("SELECT * FROM tabla");

// Procesar los datos
$datos = procesar_datos($datos);

// Generar el informe
$html = generar_informe($datos);

// Mostrar el informe en HTML
echo $html;

?>
```

**Explicación del código:**

* La función `obtener_datos_de_la_base_de_datos()` se utiliza para obtener los datos de una base de datos mediante una consulta SQL.
* La función `procesar_datos()` se utiliza para procesar los datos de la forma deseada.
* La función `generar_informe()` se utiliza para generar un informe en formato HTML a partir de los datos procesados.
* La función `addFila()` se utiliza para añadir una fila de datos al informe.
* La función `generarHTML()` se utiliza para generar el informe en formato HTML.

El código anterior se puede utilizar para generar un informe a partir de los datos de una base de datos. El informe se generará en formato HTML y se mostrará en el navegador web.