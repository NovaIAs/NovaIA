```tcl
# Este código creará un servidor web simple en TCL.

# Definimos el puerto en el que queremos que nuestro servidor web escuche.
set puerto 8080

# Creamos un nuevo socket para nuestro servidor web.
set socket [socket stream]

# Bind el socket al puerto especificado.
[socket bind $socket $puerto]

# Ponemos el socket en modo escucha.
[socket listen $socket]

# Creamos un loop infinito para manejar las solicitudes entrantes.
while {1} {
    # Aceptamos una nueva conexión.
    set conexión [socket accept $socket]

    # Leemos la solicitud del cliente.
    set solicitud [read $conexión]

    # Parseamos la solicitud del cliente.
    set partes [split $solicitud "\r\n"]
    set método [lindex $partes 0]
    set ruta [lindex $partes 1]
    set versión [lindex $partes 2]

    # Preparamos la respuesta.
    set respuesta "HTTP/1.1 200 OK\r\n"
    set respuesta "[concat $respuesta Content-Type: text/html\r\n]"
    set respuesta "[concat $respuesta \r\n]"
    set respuesta "[concat $respuesta <h1>Hola, mundo!</h1>\r\n]"

    # Enviamos la respuesta al cliente.
    [write $conexión $respuesta]

    # Cerramos la conexión con el cliente.
    [close $conexión]
}

# Cerramos el socket del servidor web.
[close $socket]
```

Este código crea un servidor web simple en TCL. El código primero define el puerto en el que el servidor web debe escuchar. Luego, crea un nuevo socket para el servidor web y lo asocia al puerto especificado. A continuación, pone el socket en modo escucha.

El código crea un loop infinito para manejar las solicitudes entrantes. En cada iteración del loop, acepta una nueva conexión de un cliente, lee la solicitud del cliente, parsea la solicitud y prepara una respuesta. La respuesta se envía al cliente y luego se cierra la conexión.

Cuando el servidor web ya no es necesario, el código cierra el socket del servidor web.