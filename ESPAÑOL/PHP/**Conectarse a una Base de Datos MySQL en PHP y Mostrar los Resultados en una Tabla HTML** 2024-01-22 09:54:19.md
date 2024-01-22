```php
<?php

// Definir constantes para los datos de conexión a la base de datos
define('DB_HOST', 'localhost');
define('DB_USER', 'root');
define('DB_PASS', '');
define('DB_NAME', 'mi_base_de_datos');

// Crear una conexión a la base de datos
$conexion = mysqli_connect(DB_HOST, DB_USER, DB_PASS, DB_NAME);

// Comprobar si la conexión se realizó correctamente
if (!$conexion) {
  die('Error al conectar a la base de datos: ' . mysqli_connect_error());
}

// Crear una consulta para obtener todas las películas de la base de datos
$consulta = 'SELECT * FROM peliculas';

// Ejecutar la consulta y obtener los resultados
$resultado = mysqli_query($conexion, $consulta);

// Comprobar si la consulta se realizó correctamente
if (!$resultado) {
  die('Error al ejecutar la consulta: ' . mysqli_error($conexion));
}

// Crear un array para almacenar las películas
$peliculas = array();

// Recorrer los resultados de la consulta y añadir cada película al array
while ($fila = mysqli_fetch_assoc($resultado)) {
  $peliculas[] = $fila;
}

// Cerrar la conexión a la base de datos
mysqli_close($conexion);

// Mostrar las películas en una tabla HTML
echo '<table border="1" cellpadding="5" cellspacing="0">';
echo '<thead>';
echo '<tr>';
echo '<th>ID</th>';
echo '<th>Título</th>';
echo '<th>Año</th>';
echo '<th>Duración</th>';
echo '</tr>';
echo '</thead>';
echo '<tbody>';
foreach ($peliculas as $pelicula) {
  echo '<tr>';
  echo '<td>' . $pelicula['id'] . '</td>';
  echo '<td>' . $pelicula['titulo'] . '</td>';
  echo '<td>' . $pelicula['año'] . '</td>';
  echo '<td>' . $pelicula['duracion'] . '</td>';
  echo '</tr>';
}
echo '</tbody>';
echo '</table>';

?>
```

Este código es un ejemplo de cómo conectar a una base de datos MySQL en PHP, ejecutar una consulta y mostrar los resultados en una tabla HTML. El código está bien documentado y utiliza buenas prácticas de programación, como definir constantes para los datos de conexión y comprobar si las consultas se realizan correctamente.