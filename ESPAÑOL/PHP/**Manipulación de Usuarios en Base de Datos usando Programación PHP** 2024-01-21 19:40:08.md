```php
<?php

// Definir constantes para la conexión a la base de datos
define('DB_HOST', 'localhost');
define('DB_USER', 'username');
define('DB_PASS', 'password');
define('DB_NAME', 'database_name');

// Definir la función para conectarse a la base de datos
function db_connect() {
  // Conectarse a la base de datos
  $conn = mysqli_connect(DB_HOST, DB_USER, DB_PASS, DB_NAME);

  // Comprobar si la conexión se ha realizado correctamente
  if (!$conn) {
    die("Error de conexión a la base de datos: " . mysqli_connect_error());
  }

  // Devolver la conexión
  return $conn;
}

// Definir la función para cerrar la conexión a la base de datos
function db_close($conn) {
  // Cerrar la conexión a la base de datos
  mysqli_close($conn);
}

// Definir la función para obtener todos los registros de la tabla "usuarios"
function get_all_users() {
  // Conectarse a la base de datos
  $conn = db_connect();

  // Preparar la consulta SQL
  $sql = "SELECT * FROM usuarios";

  // Ejecutar la consulta SQL
  $result = mysqli_query($conn, $sql);

  // Comprobar si la consulta se ha realizado correctamente
  if (!$result) {
    die("Error al obtener los usuarios: " . mysqli_error($conn));
  }

  // Convertir el resultado de la consulta en un array asociativo
  $users = mysqli_fetch_all($result, MYSQLI_ASSOC);

  // Cerrar la conexión a la base de datos
  db_close($conn);

  // Devolver los usuarios
  return $users;
}

// Definir la función para obtener un usuario por su ID
function get_user_by_id($id) {
  // Conectarse a la base de datos
  $conn = db_connect();

  // Preparar la consulta SQL
  $sql = "SELECT * FROM usuarios WHERE id = ?";

  // Preparar la sentencia SQL
  $stmt = mysqli_prepare($conn, $sql);

  // Enlazar los parámetros de la consulta SQL
  mysqli_stmt_bind_param($stmt, "i", $id);

  // Ejecutar la sentencia SQL
  mysqli_stmt_execute($stmt);

  // Obtener el resultado de la consulta SQL
  $result = mysqli_stmt_get_result($stmt);

  // Comprobar si la consulta se ha realizado correctamente
  if (!$result) {
    die("Error al obtener el usuario: " . mysqli_error($conn));
  }

  // Convertir el resultado de la consulta en un array asociativo
  $user = mysqli_fetch_assoc($result);

  // Cerrar la conexión a la base de datos
  db_close($conn);

  // Devolver el usuario
  return $user;
}

// Definir la función para crear un nuevo usuario
function create_user($nombre, $email, $password) {
  // Conectarse a la base de datos
  $conn = db_connect();

  // Preparar la consulta SQL
  $sql = "INSERT INTO usuarios (nombre, email, password) VALUES (?, ?, ?)";

  // Preparar la sentencia SQL
  $stmt = mysqli_prepare($conn, $sql);

  // Enlazar los parámetros de la consulta SQL
  mysqli_stmt_bind_param($stmt, "sss", $nombre, $email, $password);

  // Ejecutar la sentencia SQL
  mysqli_stmt_execute($stmt);

  // Obtener el ID del usuario recién creado
  $id = mysqli_insert_id($conn);

  // Cerrar la conexión a la base de datos
  db_close($conn);

  // Devolver el ID del usuario recién creado
  return $id;
}

// Definir la función para actualizar un usuario
function update_user($id, $nombre, $email, $password) {
  // Conectarse a la base de datos
  $conn = db_connect();

  // Preparar la consulta SQL
  $sql = "UPDATE usuarios SET nombre = ?, email = ?, password = ? WHERE id = ?";

  // Preparar la sentencia SQL
  $stmt = mysqli_prepare($conn, $sql);

  // Enlazar los parámetros de la consulta SQL
  mysqli_stmt_bind_param($stmt, "sssi", $nombre, $email, $password, $id);

  // Ejecutar la sentencia SQL
  mysqli_stmt_execute($stmt);

  // Cerrar la conexión a la base de datos
  db_close($conn);
}

// Definir la función para eliminar un usuario
function delete_user($id) {
  // Conectarse a la base de datos
  $conn = db_connect();

  // Preparar la consulta SQL
  $sql = "DELETE FROM usuarios WHERE id = ?";

  // Preparar la sentencia SQL
  $stmt = mysqli_prepare($conn, $sql);

  // Enlazar los parámetros de la consulta SQL
  mysqli_stmt_bind_param($stmt, "i", $id);

  // Ejecutar la sentencia SQL
  mysqli_stmt_execute($stmt);

  // Cerrar la conexión a la base de datos
  db_close($conn);
}

// Definir la función para comprobar si un usuario existe
function user_exists($email) {
  // Conectarse a la base de datos
  $conn = db_connect();

  // Preparar la consulta SQL
  $sql = "SELECT * FROM usuarios WHERE email = ?";

  // Preparar la sentencia SQL
  $stmt = mysqli_prepare($conn, $sql);

  // Enlazar los parámetros de la consulta SQL
  mysqli_stmt_bind_param($stmt, "s", $email);

  // Ejecutar la sentencia SQL
  mysqli_stmt_execute($stmt);

  // Obtener el resultado de la consulta SQL
  $result = mysqli_stmt_get_result($stmt);

  // Comprobar si la consulta se ha realizado correctamente
  if (!$result) {
    die("Error al comprobar si el usuario existe: " . mysqli_error($conn));
  }

  // Comprobar si el usuario existe
  $user_exists = (mysqli_num_rows($result) > 0);

  // Cerrar la conexión a la base de datos
  db_close($conn);

  // Devolver si el usuario existe
  return $user_exists;
}

// Definir la función para iniciar sesión
function login($email, $password) {
  // Conectarse a la base de datos
  $conn = db_connect();

  // Preparar la consulta SQL
  $sql = "SELECT * FROM usuarios WHERE email = ?";

  // Preparar la sentencia SQL
  $stmt = mysqli_prepare($conn, $sql);

  // Enlazar los parámetros de la consulta SQL
  mysqli_stmt_bind_param($stmt, "s", $email);

  // Ejecutar la sentencia SQL
  mysqli_stmt_execute($stmt);

  // Obtener el resultado de la consulta SQL
  $result = mysqli_stmt_get_result($stmt);

  // Comprobar si la consulta se ha realizado correctamente
  if (!$result) {
    die("Error al iniciar sesión: " . mysqli_error($conn));
  }

  // Comprobar si el usuario existe
  $user = mysqli_fetch_assoc($result);

  // Comprobar