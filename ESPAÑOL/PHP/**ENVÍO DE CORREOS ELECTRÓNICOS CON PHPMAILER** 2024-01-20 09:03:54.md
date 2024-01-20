```php
<?php

// Cargar una librería externa para generar y enviar correos electrónicos
require_once 'vendor/autoload.php';
use PHPMailer\PHPMailer\PHPMailer;
use PHPMailer\PHPMailer\Exception;

// Crear una conexión a la base de datos
$mysqli = new mysqli("localhost", "usuario", "contraseña", "base_de_datos");

// Consultar los usuarios que cumplen un criterio específico
$resultado = $mysqli->query("SELECT * FROM usuarios WHERE activo = 1");

// Recorrer los resultados y generar el cuerpo del correo electrónico
$cuerpo = "<ul>";
while ($fila = $resultado->fetch_assoc()) {
    $cuerpo .= "<li>{$fila['nombre']} {$fila['apellido']}</li>";
}
$cuerpo .= "</ul>";

// Generar un objeto PHPMailer para enviar el correo electrónico
$mail = new PHPMailer(true);

try {
    // Configurar las propiedades del correo electrónico
    $mail->CharSet = 'UTF-8';
    $mail->isSMTP();
    $mail->Host = 'smtp.example.com';
    $mail->SMTPAuth = true;
    $mail->Username = 'usuario@example.com';
    $mail->Password = 'contraseña';
    $mail->SMTPSecure = 'tls';
    $mail->Port = 587;

    // Añadir el remitente y el destinatario
    $mail->setFrom('usuario@example.com', 'Nombre del remitente');
    $mail->addAddress('destinatario@example.com', 'Nombre del destinatario');

    // Añadir el asunto y el cuerpo del correo electrónico
    $mail->Subject = 'Listado de usuarios activos';
    $mail->Body = $cuerpo;

    // Enviar el correo electrónico
    $mail->send();

    // Mostrar un mensaje de éxito
    echo "El correo electrónico se ha enviado correctamente.";
} catch (Exception $e) {
    // Mostrar un mensaje de error
    echo "Error al enviar el correo electrónico: {$mail->ErrorInfo}";
}

// Cerrar la conexión a la base de datos
$mysqli->close();

?>
```

Explicación del código:

* En primer lugar, se carga la librería externa PHPMailer, que se utiliza para generar y enviar correos electrónicos.
* A continuación, se establece una conexión a la base de datos mediante la clase mysqli.
* Seguidamente, se ejecuta una consulta a la base de datos para obtener los usuarios que cumplen un determinado criterio.
* El resultado de la consulta se recorre y se genera el cuerpo del correo electrónico, que consiste en una lista de los usuarios activos.
* Después, se crea un objeto PHPMailer para enviar el correo electrónico. Se configuran las propiedades del correo electrónico, como el remitente, el destinatario, el asunto y el cuerpo.
* Una vez configurado el objeto PHPMailer, se envía el correo electrónico.
* Por último, se muestra un mensaje de éxito o de error en función de si el correo electrónico se ha enviado correctamente o no.