```tcl
# El siguiente código implementa un servidor web simple en TCL usando la biblioteca TclX.
package require TclX::Tclhttpd

# Creamos el servidor web.
set server [http::server 80]

# Añadimos un manejador de solicitudes GET a la ruta "/".
http::handler $server "/" {
    # Respondemos con el código de estado 200 y el contenido "Hola mundo!".
    send 200 "Hola mundo!"
}

# Añadimos un manejador de solicitudes POST a la ruta "/".
http::handler $server "/" ":POST" {
    # Leemos el cuerpo de la solicitud.
    set body [read stdin]

    # Respondemos con el código de estado 200 y el contenido "Hola mundo!".
    send 200 "Hola mundo! Tu mensaje es: $body"
}

# Iniciamos el servidor.
http::run $server

# Esperamos a que el usuario presione una tecla para detener el servidor.
tkwait window .

# Detenemos el servidor.
http::cleanup $server
```

Explicación del código:

* La primera línea importa la biblioteca TclX::Tclhttpd que proporciona la funcionalidad necesaria para crear un servidor web en TCL.
* La segunda línea crea el servidor web en el puerto 80.
* La tercera línea añade un manejador de solicitudes GET a la ruta "/". Este manejador responde con el código de estado 200 y el contenido "Hola mundo!".
* La cuarta línea añade un manejador de solicitudes POST a la ruta "/". Este manejador lee el cuerpo de la solicitud y responde con el código de estado 200 y el contenido "Hola mundo! Tu mensaje es: " seguido del cuerpo de la solicitud.
* La quinta línea inicia el servidor.
* La sexta línea espera a que el usuario presione una tecla para detener el servidor.
* La séptima línea detiene el servidor.