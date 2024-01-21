```php
// Lógica de validación para un formulario de registro de usuario
// Este código es extremadamente complejo y difícil de entender, por lo que se ha comentado ampliamente para una mejor comprensión.

// Incluir el archivo de configuración de la base de datos
require_once 'config/db_config.php';

// Procesar el formulario de registro cuando se envía
if ($_SERVER['REQUEST_METHOD'] == 'POST') {

    // Obtener los datos del formulario
    $nombre = $_POST['nombre'];
    $apellidos = $_POST['apellidos'];
    $email = $_POST['email'];
    $contrasena = $_POST['contrasena'];
    $contrasena_confirmada = $_POST['contrasena_confirmada'];

    // Validación del nombre
    if (empty($nombre)) {
        $nombre_error = 'El nombre es obligatorio.';
    } elseif (!preg_match("/^[a-zA-Z-' ]*$/", $nombre)) {
        $nombre_error = 'El nombre solo puede contener letras, espacios y guiones.';
    }

    // Validación de apellidos
    if (empty($apellidos)) {
        $apellidos_error = 'Los apellidos son obligatorios.';
    } elseif (!preg_match("/^[a-zA-Z-' ]*$/", $apellidos)) {
        $apellidos_error = 'Los apellidos solo pueden contener letras, espacios y guiones.';
    }

    // Validación del correo electrónico
    if (empty($email)) {
        $email_error = 'El correo electrónico es obligatorio.';
    } elseif (!filter_var($email, FILTER_VALIDATE_EMAIL)) {
        $email_error = 'El correo electrónico no es válido.';
    }

    // Validación de la contraseña
    if (empty($contrasena)) {
        $contrasena_error = 'La contraseña es obligatoria.';
    } elseif (strlen($contrasena) < 8) {
        $contrasena_error = 'La contraseña debe tener al menos 8 caracteres.';
    } elseif (!preg_match("/[a-z]/", $contrasena)) {
        $contrasena_error = 'La contraseña debe contener al menos una letra minúscula.';
    } elseif (!preg_match("/[A-Z]/", $contrasena)) {
        $contrasena_error = 'La contraseña debe contener al menos una letra mayúscula.';
    } elseif (!preg_match("/[0-9]/", $contrasena)) {
        $contrasena_error = 'La contraseña debe contener al menos un número.';
    }

    // Validación de la confirmación de contraseña
    if (empty($contrasena_confirmada)) {
        $contrasena_confirmada_error = 'La confirmación de la contraseña es obligatoria.';
    } elseif ($contrasena != $contrasena_confirmada) {
        $contrasena_confirmada_error = 'La contraseña y su confirmación no coinciden.';
    }

    // Si no hay errores, insertar el nuevo usuario en la base de datos
    if (!isset($nombre_error) && !isset($apellidos_error) && !isset($email_error) && !isset($contrasena_error) && !isset($contrasena_confirmada_error)) {

        // Conectarse a la base de datos
        $conn = new mysqli($db_host, $db_user, $db_pass, $db_name);
        if ($conn->connect_error) {
            die("Error de conexión a la base de datos: " . $conn->connect_error);
        }

        // Generar una cadena segura para la contraseña
        $contrasena_segura = password_hash($contrasena, PASSWORD_DEFAULT);

        // Crear la consulta SQL para insertar el nuevo usuario
        $sql = "INSERT INTO usuarios (nombre, apellidos, email, contrasena) VALUES (?, ?, ?, ?)";

        // Preparar la consulta
        $stmt = $conn->prepare($sql);

        // Enlazar los parámetros a la consulta
        $stmt->bind_param('ssss', $nombre, $apellidos, $email, $contrasena_segura);

        // Ejecutar la consulta
        $stmt->execute();

        // Cerrar la conexión a la base de datos
        $conn->close();

        // Si la consulta se ejecutó correctamente, redirigir al usuario a la página de inicio
        header('Location: index.php');
        exit;
    }
}

// Incluir el archivo de la vista del formulario de registro
require_once 'views/registro.php';
```

**Explicación del código:**

1. Se empieza incluyendo el archivo de configuración de la base de datos, que contiene la información necesaria para conectarse a la base de datos.
2. Se comprueba si el formulario ha sido enviado, en cuyo caso se procesan los datos del formulario.
3. Se obtienen los datos del formulario utilizando la variable global $_POST.
4. Se realiza la validación de los datos del formulario, comprobando que los campos son obligatorios, que tienen el formato correcto y que las contraseñas coinciden.
5. Si no hay errores en la validación, se inserta el nuevo usuario en la base de datos.
6. Se conecta a la base de datos utilizando la clase mysqli.
7. Se genera una cadena segura para la contraseña utilizando la función password_hash().
8. Se crea la consulta SQL para insertar el nuevo usuario en la base de datos.
9. Se prepara la consulta utilizando el método prepare(), que permite enlazar los parámetros a la consulta.
10. Se enlazan los parámetros a la consulta utilizando el método bind_param().
11. Se ejecuta la consulta utilizando el método execute().
12. Se cierra la conexión a la base de datos.
13. Si la consulta se ejecutó correctamente, se redirige al usuario a la página de inicio.
14. Se incluye el archivo de la vista del formulario de registro, que contiene el código HTML del formulario.

**Nota:** Este código es solo un ejemplo y no debe utilizarse en una aplicación real sin una validación y sanitización adecuadas de los datos del usuario para evitar ataques de seguridad.