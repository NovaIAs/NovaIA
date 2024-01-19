```php
<?php
// Definir la configuración de la base de datos
$host = "localhost";
$database = "nombre_base_datos";
$user = "nombre_usuario";
$password = "contraseña";

// Conectar a la base de datos
$conn = mysqli_connect($host, $user, $password, $database);

// Comprobar la conexión
if (!$conn) {
  die("Error de conexión: " . mysqli_connect_error());
}

// Definir la consulta SQL
$query = "SELECT * FROM usuarios WHERE rol = 'administrador'";

// Ejecutar la consulta
$result = mysqli_query($conn, $query);

// Comprobar si hay resultados
if (mysqli_num_rows($result) > 0) {
  // Recorrer los resultados
  while ($row = mysqli_fetch_assoc($result)) {
    // Obtener los datos de cada fila
    $id = $row["id"];
    $nombre = $row["nombre"];
    $correo = $row["correo"];

    // Mostrar los datos en una tabla HTML
    echo "<tr>";
    echo "<td>" . $id . "</td>";
    echo "<td>" . $nombre . "</td>";
    echo "<td>" . $correo . "</td>";
    echo "</tr>";
  }
} else {
  // No hay resultados
  echo "No hay usuarios administradores en la base de datos.";
}

// Cerrar la conexión a la base de datos
mysqli_close($conn);
?>
```

Este código PHP realiza una consulta a una base de datos MySQL para obtener los usuarios que tienen el rol de administrador.

A continuación, se explican los pasos del código:

1. Se definen las variables para la configuración de la base de datos: el host, el nombre de la base de datos, el usuario y la contraseña.
2. Se establece la conexión a la base de datos utilizando la función `mysqli_connect()`.
3. Se comprueba si la conexión se ha realizado correctamente con `mysqli_connect_error()`. Si hay algún error, se muestra en pantalla.
4. Se define la consulta SQL que se va a ejecutar en la base de datos. En este caso, la consulta es `SELECT * FROM usuarios WHERE rol = 'administrador'`.
5. Se ejecuta la consulta utilizando la función `mysqli_query()`.
6. Se comprueba si hay resultados en la consulta utilizando la función `mysqli_num_rows()`. Si hay resultados, se recorren utilizando un bucle `while`.
7. Para cada resultado, se obtienen los datos de las columnas `id`, `nombre` y `correo` utilizando la función `mysqli_fetch_assoc()`.
8. Se muestran los datos en una tabla HTML utilizando la función `echo`.
9. Si no hay resultados en la consulta, se muestra un mensaje en pantalla indicando que no hay usuarios administradores en la base de datos.
10. Finalmente, se cierra la conexión a la base de datos utilizando la función `mysqli_close()`.