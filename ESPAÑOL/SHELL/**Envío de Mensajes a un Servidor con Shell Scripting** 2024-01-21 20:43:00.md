```
#!/bin/bash

# Creamos una función que nos permita mostrar un mensaje de error por consola
function mostrar_error() {
  echo -e "\033[31mERROR:\033[0m $1"
}

# Creamos una función que nos permita mostrar un mensaje de información por consola
function mostrar_info() {
  echo -e "\033[32mINFO:\033[0m $1"
}

# Creamos una variable para almacenar el nombre del fichero de configuración
fichero_configuracion="config.ini"

# Comprobamos si existe el fichero de configuración
if [ ! -f $fichero_configuracion ]; then
  # Si no existe, mostramos un mensaje de error y salimos del script
  mostrar_error "No se ha encontrado el fichero de configuración $fichero_configuracion"
  exit 1
fi

# Cargamos el fichero de configuración en una variable de entorno
source $fichero_configuracion

# Creamos una variable para almacenar la dirección IP del servidor
ip_servidor=$SERVER_IP

# Creamos una variable para almacenar el puerto del servidor
puerto_servidor=$SERVER_PORT

# Creamos una variable para almacenar el mensaje que queremos enviar al servidor
mensaje="Hola, mundo!"

# Creamos un socket de conexión TCP
socket=$(socat TCP-CONNECT:$ip_servidor:$puerto_servidor)

# Comprobamos si se ha creado el socket correctamente
if [ $? -ne 0 ]; then
  # Si no se ha creado correctamente, mostramos un mensaje de error y salimos del script
  mostrar_error "No se ha podido crear el socket de conexión TCP"
  exit 1
fi

# Enviamos el mensaje al servidor a través del socket
echo $mensaje > $socket

# Cerramos el socket de conexión TCP
socat -d $socket

# Mostramos un mensaje de información por consola
mostrar_info "Mensaje enviado al servidor correctamente"

```

**Explicación del código:**

* El script comienza definiendo dos funciones:

    * `mostrar_error`: Esta función se utiliza para mostrar un mensaje de error por consola.
    * `mostrar_info`: Esta función se utiliza para mostrar un mensaje de información por consola.

* El script carga el fichero de configuración `config.ini` que contiene las siguientes variables:

    * `SERVER_IP`: Dirección IP del servidor.
    * `SERVER_PORT`: Puerto del servidor.

* El script crea un socket de conexión TCP utilizando la orden `socat`.
* El script comprueba si se ha creado el socket correctamente.
* Si el socket se ha creado correctamente, el script envía el mensaje al servidor a través del socket utilizando la orden `echo`.
* El script cierra el socket de conexión TCP utilizando la orden `socat -d`.
* El script muestra un mensaje de información por consola indicando que el mensaje se ha enviado al servidor correctamente.