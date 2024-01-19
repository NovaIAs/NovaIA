```tcl
# Importa la biblioteca de manejo de redes
package require Tcl 8.5
package require Tk 8.5
package require Tcl::Http 2.0

# Crea una instancia del cliente HTTP
http::Client client

# Establece las opciones del cliente HTTP
client configure -timeout 10000

# Envía una solicitud GET a la URL especificada
response [http::getclient $client http://www.example.com/]

# Comprueba el código de estado de la respuesta
if {[http::status $response] != 200} {
    puts "Error: HTTP error code [http::status $response]"
    exit 1
}

# Obtiene el cuerpo de la respuesta
body [http::content $response]

# Imprime el cuerpo de la respuesta
puts $body

# Cierra la instancia del cliente HTTP
http::closeclient $client
```

Explicación:

* La primera línea importa la biblioteca de manejo de redes Tcl.
* La segunda línea importa la biblioteca Tk, que se utiliza para crear interfaces gráficas de usuario.
* La tercera línea importa la biblioteca Tcl::Http, que se utiliza para realizar peticiones HTTP.
* La cuarta línea crea una instancia del cliente HTTP.
* La quinta línea establece las opciones del cliente HTTP, en este caso el tiempo de espera de la conexión.
* La sexta línea envía una solicitud GET a la URL especificada.
* La séptima línea comprueba el código de estado de la respuesta. Si el código de estado no es 200, se imprime un mensaje de error y se cierra el programa.
* La octava línea obtiene el cuerpo de la respuesta.
* La novena línea imprime el cuerpo de la respuesta.
* La décima línea cierra la instancia del cliente HTTP.

Este código es un ejemplo de cómo utilizar la biblioteca Tcl::Http para realizar peticiones HTTP en Tcl. Puede utilizarse para crear una variedad de aplicaciones web, como clientes de correo electrónico, navegadores web y aplicaciones de comercio electrónico.