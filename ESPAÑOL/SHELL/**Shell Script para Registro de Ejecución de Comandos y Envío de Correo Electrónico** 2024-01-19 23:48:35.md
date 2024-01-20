```
#!/bin/bash

# Definir variables
directorio_actual=$(pwd)
fichero_temporal="/tmp/fichero_temporal.txt"
fichero_log="/var/log/script.log"
comando="comando_a_ejecutar"

# Crear un fichero temporal
touch $fichero_temporal

# Escribir en el fichero temporal
echo "Línea 1" >> $fichero_temporal
echo "Línea 2" >> $fichero_temporal
echo "Línea 3" >> $fichero_temporal

# Ejecutar un comando y redirigir la salida al fichero temporal
$comando >> $fichero_temporal

# Comprobar si el comando se ejecutó correctamente
if [ $? -eq 0 ]; then
  echo "El comando se ejecutó correctamente" >> $fichero_log
else
  echo "El comando falló" >> $fichero_log
fi

# Leer el contenido del fichero temporal y mostrarlo por pantalla
cat $fichero_temporal

# Eliminar el fichero temporal
rm $fichero_temporal

# Enviar un correo electrónico con el contenido del fichero log
mail -s "Informe del script" correo@ejemplo.com < $fichero_log

```

Explicación:

* El script comienza definiendo las variables necesarias para su ejecución.

* A continuación, crea un fichero temporal para almacenar información durante la ejecución del script.

* El script escribe algunas líneas de texto en el fichero temporal.

* Luego, ejecuta un comando y redirige la salida del comando al fichero temporal.

* El script comprueba si el comando se ejecutó correctamente. Si el comando se ejecutó correctamente, muestra un mensaje en el fichero de registro. Si el comando falló, muestra un mensaje de error en el fichero de registro.

* El script lee el contenido del fichero temporal y lo muestra por pantalla.

* Finalmente, el script elimina el fichero temporal y envía un correo electrónico con el contenido del fichero de registro a una dirección de correo electrónico especificada.