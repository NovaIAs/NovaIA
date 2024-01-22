```php
// Función para generar una contraseña segura
function generarContraseñaSegura($longitud) {
    $caracteres = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()';
    $contraseña = '';
    for ($i = 0; $i < $longitud; $i++) {
        $contraseña .= $caracteres[rand(0, strlen($caracteres) - 1)];
    }
    return $contraseña;
}

// Función para enviar un correo electrónico
function enviarCorreo($destinatario, $asunto, $mensaje) {
    $headers = 'From: noreply@example.com' . "\r\n" .
        'Reply-To: noreply@example.com' . "\r\n" .
        'X-Mailer: PHP/' . phpversion();

    return mail($destinatario, $asunto, $mensaje, $headers);
}

// Función para crear un usuario en la base de datos
function crearUsuario($nombre, $apellido, $correo, $contraseña) {
    $conn = new mysqli("localhost", "root", "", "mi_base_de_datos");
    if ($conn->connect_error) {
        die("Error de conexión: " . $conn->connect_error);
    }

    $stmt = $conn->prepare("INSERT INTO usuarios (nombre, apellido, correo, contraseña) VALUES (?, ?, ?, ?)");
    $stmt->bind_param("ssss", $nombre, $apellido, $correo, $contraseña);
    $stmt->execute();
    $conn->close();
}

// Función para iniciar sesión de un usuario
function iniciarSesion($correo, $contraseña) {
    $conn = new mysqli("localhost", "root", "", "mi_base_de_datos");
    if ($conn->connect_error) {
        die("Error de conexión: " . $conn->connect_error);
    }

    $stmt = $conn->prepare("SELECT * FROM usuarios WHERE correo = ? AND contraseña = ?");
    $stmt->bind_param("ss", $correo, $contraseña);
    $stmt->execute();
    $resultado = $stmt->get_result();
    $usuario = $resultado->fetch_assoc();
    $conn->close();

    return $usuario;
}

// Función para cerrar sesión de un usuario
function cerrarSesion() {
    session_start();
    session_unset();
    session_destroy();
}

// Función para comprobar si un usuario está logueado
function estaLogueado() {
    session_start();
    return isset($_SESSION['usuario']);
}

// Función para obtener los datos del usuario logueado
function obtenerUsuarioLogueado() {
    session_start();
    return $_SESSION['usuario'];
}

// Función para mostrar un mensaje de error
function mostrarError($mensaje) {
    echo '<div class="alert alert-danger" role="alert">' . $mensaje . '</div>';
}

// Función para mostrar un mensaje de éxito
function mostrarExito($mensaje) {
    echo '<div class="alert alert-success" role="alert">' . $mensaje . '</div>';
}
```