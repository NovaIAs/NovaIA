```php
<?php

// Función para conectar a la base de datos
function conectar_bd() {
    // Parámetros de conexión
    $host = 'localhost';
    $usuario = 'root';
    $contraseña = '';
    $base_de_datos = 'mi_bd';

    // Crear conexión
    $conexion = mysqli_connect($host, $usuario, $contraseña, $base_de_datos);

    // Verificar conexión
    if (!$conexion) {
        die('Error al conectar a la base de datos');
    }

    // Retornar conexión
    return $conexion;
}

// Función para obtener todos los usuarios de la base de datos
function obtener_usuarios() {
    // Conectar a la base de datos
    $conexion = conectar_bd();

    // Crear consulta
    $consulta = 'SELECT * FROM usuarios';

    // Ejecutar consulta
    $resultado = mysqli_query($conexion, $consulta);

    // Verificar resultado
    if (!$resultado) {
        die('Error al obtener los usuarios');
    }

    // Crear array de usuarios
    $usuarios = array();

    // Iterar sobre los resultados y agregarlos al array
    while ($fila = mysqli_fetch_assoc($resultado)) {
        $usuarios[] = $fila;
    }

    // Cerrar conexión
    mysqli_close($conexion);

    // Retornar array de usuarios
    return $usuarios;
}

// Función para obtener un usuario por su ID
function obtener_usuario($id) {
    // Conectar a la base de datos
    $conexion = conectar_bd();

    // Crear consulta
    $consulta = "SELECT * FROM usuarios WHERE id = $id";

    // Ejecutar consulta
    $resultado = mysqli_query($conexion, $consulta);

    // Verificar resultado
    if (!$resultado) {
        die('Error al obtener el usuario');
    }

    // Obtener el primer resultado
    $usuario = mysqli_fetch_assoc($resultado);

    // Cerrar conexión
    mysqli_close($conexion);

    // Retornar usuario
    return $usuario;
}

// Función para crear un nuevo usuario
function crear_usuario($nombre, $email, $contraseña) {
    // Conectar a la base de datos
    $conexion = conectar_bd();

    // Crear consulta
    $consulta = "INSERT INTO usuarios (nombre, email, contraseña) VALUES ('$nombre', '$email', '$contraseña')";

    // Ejecutar consulta
    $resultado = mysqli_query($conexion, $consulta);

    // Verificar resultado
    if (!$resultado) {
        die('Error al crear el usuario');
    }

    // Obtener el ID del nuevo usuario
    $id = mysqli_insert_id($conexion);

    // Cerrar conexión
    mysqli_close($conexion);

    // Retornar ID del nuevo usuario
    return $id;
}

// Función para actualizar un usuario
function actualizar_usuario($id, $nombre, $email, $contraseña) {
    // Conectar a la base de datos
    $conexion = conectar_bd();

    // Crear consulta
    $consulta = "UPDATE usuarios SET nombre = '$nombre', email = '$email', contraseña = '$contraseña' WHERE id = $id";

    // Ejecutar consulta
    $resultado = mysqli_query($conexion, $consulta);

    // Verificar resultado
    if (!$resultado) {
        die('Error al actualizar el usuario');
    }

    // Cerrar conexión
    mysqli_close($conexion);
}

// Función para eliminar un usuario
function eliminar_usuario($id) {
    // Conectar a la base de datos
    $conexion = conectar_bd();

    // Crear consulta
    $consulta = "DELETE FROM usuarios WHERE id = $id";

    // Ejecutar consulta
    $resultado = mysqli_query($conexion, $consulta);

    // Verificar resultado
    if (!$resultado) {
        die('Error al eliminar el usuario');
    }

    // Cerrar conexión
    mysqli_close($conexion);
}

// Obtener todos los usuarios
$usuarios = obtener_usuarios();

// Mostrar los usuarios
foreach ($usuarios as $usuario) {
    echo 'ID: ' . $usuario['id'] . '<br>';
    echo 'Nombre: ' . $usuario['nombre'] . '<br>';
    echo 'Email: ' . $usuario['email'] . '<br>';
    echo 'Contraseña: ' . $usuario['contraseña'] . '<br><br>';
}

// Obtener un usuario por su ID
$usuario = obtener_usuario(1);

// Mostrar el usuario
echo 'ID: ' . $usuario['id'] . '<br>';
echo 'Nombre: ' . $usuario['nombre'] . '<br>';
echo 'Email: ' . $usuario['email'] . '<br>';
echo 'Contraseña: ' . $usuario['contraseña'] . '<br><br>';

// Crear un nuevo usuario
$id = crear_usuario('Juan Pérez', 'juan@example.com', '123456');

// Obtener el nuevo usuario
$usuario = obtener_usuario($id);

// Mostrar el nuevo usuario
echo 'ID: ' . $usuario['id'] . '<br>';
echo 'Nombre: ' . $usuario['nombre'] . '<br>';
echo 'Email: ' . $usuario['email'] . '<br>';
echo 'Contraseña: ' . $usuario['contraseña'] . '<br><br>';

// Actualizar el nuevo usuario
actualizar_usuario($id, 'Juan García', 'juan@example.com', '654321');

// Obtener el usuario actualizado
$usuario = obtener_usuario($id);

// Mostrar el usuario actualizado
echo 'ID: ' . $usuario['id'] . '<br>';
echo 'Nombre: ' . $usuario['nombre'] . '<br>';
echo 'Email: ' . $usuario['email'] . '<br>';
echo 'Contraseña: ' . $usuario['contraseña'] . '<br><br>';

// Eliminar el usuario actualizado
eliminar_usuario($id);

// Verificar que el usuario ha sido eliminado
$usuario = obtener_usuario($id);

// Mostrar el usuario eliminado
echo 'Usuario eliminado: ' . $usuario;

```

Este código es un ejemplo de cómo conectar a una base de datos MySQL, obtener, crear, actualizar y eliminar usuarios. El código utiliza funciones para encapsular la lógica de cada operación y facilitar su reutilización.

El código también utiliza comentarios para explicar cada parte del código y facilitar su comprensión.

Este código es un ejemplo de cómo crear un sistema CRUD (Crear, Leer, Actualizar, Eliminar) en PHP.