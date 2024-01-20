```shell
#!/bin/bash

# Declaración de variables
URL="http://www.ejemplo.com"
FILE="datos.txt"
USER="usuario"
PASS="contraseña"
EMAIL="usuario@ejemplo.com"

# Importación de la biblioteca "wget"
. /usr/local/bin/wgetrc

# Descarga de datos desde una URL
wget -O $FILE $URL

# Comprobación de la descarga
if [ $? -ne 0 ]; then
  echo "Error al descargar el archivo: $FILE"
  exit 1
fi

# Conexión a un servidor SSH remoto
ssh -l $USER $URL

# Ejecución de comandos en el servidor remoto
ssh -t $USER@$URL <<EOF
  cd /var/www/html
  tar -zxvf $FILE
  rm -f $FILE
EOF

# Desconexión del servidor remoto
ssh -t $USER@$URL <<EOF
  exit
EOF

# Envío de un correo electrónico
mail -s "Datos descargados" $EMAIL < $FILE

# Eliminación del archivo descargado
rm -f $FILE

# Impresión de un mensaje de éxito
echo "Datos descargados y enviados por correo electrónico correctamente."
```

**Explicación del código:**

1. **Declaración de variables:** Se declaran varias variables que se utilizarán a lo largo del código.

2. **Importación de la biblioteca "wget":** Se importa la biblioteca "wget" para poder utilizar el comando `wget` para descargar archivos desde una URL.

3. **Descarga de datos desde una URL:** Se utiliza el comando `wget` para descargar los datos desde la URL especificada en la variable `URL` y guardarlos en el archivo especificado en la variable `FILE`.

4. **Comprobación de la descarga:** Se comprueba si la descarga se realizó correctamente mediante el código de salida del comando `wget`. Si el código de salida es distinto de 0, se muestra un mensaje de error y se sale del script.

5. **Conexión a un servidor SSH remoto:** Se utiliza el comando `ssh` para conectarse al servidor SSH remoto especificado en la variable `URL` utilizando el usuario y la contraseña especificados en las variables `USER` y `PASS`.

6. **Ejecución de comandos en el servidor remoto:** Una vez conectado al servidor remoto, se ejecutan varios comandos mediante la instrucción `ssh -t`. Estos comandos incluyen la navegación al directorio `/var/www/html`, la extracción del archivo descargado (`$FILE`) utilizando el comando `tar`, la eliminación del archivo descargado y la salida de la sesión SSH.

7. **Desconexión del servidor remoto:** Se utiliza el comando `ssh -t` para desconectarse del servidor remoto.

8. **Envío de un correo electrónico:** Se utiliza el comando `mail` para enviar un correo electrónico a la dirección especificada en la variable `EMAIL` adjuntando el archivo especificado en la variable `FILE`.

9. **Eliminación del archivo descargado:** Se utiliza el comando `rm` para eliminar el archivo descargado (`$FILE`).

10. **Impresión de un mensaje de éxito:** Se muestra un mensaje de éxito indicando que los datos se han descargado y enviado por correo electrónico correctamente.