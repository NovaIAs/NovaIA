```php
<?php
// Definir los datos de la conexión a la base de datos
$servername = "localhost";
$username = "root";
$password = "";
$dbname = "mi_base_de_datos";

// Crear la conexión a la base de datos
$conn = new mysqli($servername, $username, $password, $dbname);

// Comprobar si la conexión se ha establecido correctamente
if ($conn->connect_error) {
  die("Error al conectar con la base de datos: " . $conn->connect_error);
}

// Definir la consulta SQL para obtener los datos de la tabla "usuarios"
$sql = "SELECT * FROM usuarios";

// Ejecutar la consulta SQL
$result = $conn->query($sql);

// Comprobar si la consulta se ha ejecutado correctamente
if (!$result) {
  die("Error al ejecutar la consulta: " . $conn->error);
}

// Crear una matriz para almacenar los datos de los usuarios
$usuarios = array();

// Recorrer los resultados de la consulta y almacenar los datos en la matriz
while ($row = $result->fetch_assoc()) {
  $usuarios[] = $row;
}

// Cerrar la conexión a la base de datos
$conn->close();

// Mostrar los datos de los usuarios en una tabla HTML
echo "<table border='1'>";
echo "<thead>";
echo "<tr>";
echo "<th>ID</th>";
echo "<th>Nombre</th>";
echo "<th>Email</th>";
echo "</tr>";
echo "</thead>";
echo "<tbody>";
foreach ($usuarios as $usuario) {
  echo "<tr>";
  echo "<td>" . $usuario['id'] . "</td>";
  echo "<td>" . $usuario['nombre'] . "</td>";
  echo "<td>" . $usuario['email'] . "</td>";
  echo "</tr>";
}
echo "</tbody>";
echo "</table>";
?>
```

Explicación:

Este código en PHP realiza una serie de operaciones para conectar a una base de datos, ejecutar una consulta SQL, obtener los datos de los usuarios y mostrarlos en una tabla HTML.

1. Definir los datos de la conexión a la base de datos:

- `$servername`: El nombre del servidor donde se encuentra la base de datos.
- `$username`: El nombre de usuario para acceder a la base de datos.
- `$password`: La contraseña para acceder a la base de datos.
- `$dbname`: El nombre de la base de datos a la que queremos conectarnos.

2. Crear la conexión a la base de datos:

- Utilizamos la clase `mysqli` para crear una nueva conexión a la base de datos.
- Pasamos los datos de la conexión como argumentos al constructor de la clase `mysqli`.
- Si la conexión se establece correctamente, almacenamos el objeto `mysqli` en la variable `$conn`.

3. Comprobar si la conexión se ha establecido correctamente:

- Comprobamos si la propiedad `connect_error` del objeto `$conn` tiene algún valor.
- Si tiene algún valor, significa que se ha producido un error al conectar con la base de datos y mostramos un mensaje de error.

4. Definir la consulta SQL para obtener los datos de la tabla "usuarios":

- Creamos una variable `$sql` que contiene la consulta SQL que queremos ejecutar.
- En este caso, la consulta SQL selecciona todos los campos de la tabla "usuarios".

5. Ejecutar la consulta SQL:

- Utilizamos el método `query()` del objeto `$conn` para ejecutar la consulta SQL.
- El resultado de la consulta se almacena en la variable `$result`.

6. Comprobar si la consulta se ha ejecutado correctamente:

- Comprobamos si la variable `$result` es `false`.
- Si es `false`, significa que se ha producido un error al ejecutar la consulta y mostramos un mensaje de error.

7. Crear una matriz para almacenar los datos de los usuarios:

- Creamos una matriz vacía llamada `$usuarios` para almacenar los datos de los usuarios.

8. Recorrer los resultados de la consulta y almacenar los datos en la matriz:

- Utilizamos un bucle `while` para recorrer los resultados de la consulta.
- En cada iteración del bucle, llamamos al método `fetch_assoc()` del objeto `$result` para obtener una fila de resultados en forma de matriz asociativa.
- Añadimos la fila de resultados a la matriz `$usuarios`.

9. Cerrar la conexión a la base de datos:

- Cerramos la conexión a la base de datos llamando al método `close()` del objeto `$conn`.

10. Mostrar los datos de los usuarios en una tabla HTML:

- Creamos una tabla HTML para mostrar los datos de los usuarios.
- La tabla tiene dos columnas: "ID" y "Nombre".
- Recorremos la matriz `$usuarios` y creamos una fila para cada usuario.
- En cada fila, mostramos el ID y el nombre del usuario.