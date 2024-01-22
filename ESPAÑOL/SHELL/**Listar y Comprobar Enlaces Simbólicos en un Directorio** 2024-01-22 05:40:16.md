```bash
#!/bin/bash

# Definición de variables
directorio_actual=`pwd`
fichero_temporal=`mktemp`
fichero_salida="fichero_salida.txt"

# Buscar ficheros en el directorio actual y subdirectorios
find $directorio_actual -type f -name "*.txt" > $fichero_temporal

# Leer el fichero temporal línea por línea
while read linea; do
  # Obtener el nombre del fichero actual
  fichero_actual=$linea

  # Comprobar si el fichero actual es un enlace simbólico
  if [ -L $fichero_actual ]; then
    # Obtener el destino del enlace simbólico
    destino_enlace=$(readlink -f $fichero_actual)

    # Comprobar si el destino del enlace simbólico existe
    if [ -e $destino_enlace ]; then
      # Si el destino del enlace simbólico existe, escribir el nombre del fichero actual y el destino del enlace simbólico en el fichero de salida
      echo "$fichero_actual -> $destino_enlace" >> $fichero_salida
    else
      # Si el destino del enlace simbólico no existe, escribir el nombre del fichero actual y un mensaje de error en el fichero de salida
      echo "$fichero_actual -> Enlace simbólico roto" >> $fichero_salida
    fi
  else
    # Si el fichero actual no es un enlace simbólico, escribir el nombre del fichero actual en el fichero de salida
    echo $fichero_actual >> $fichero_salida
  fi
done < $fichero_temporal

# Eliminar el fichero temporal
rm $fichero_temporal

# Mostrar el contenido del fichero de salida
cat $fichero_salida
```

Explicación del código:

* La primera línea del código es la línea shebang, que indica al sistema operativo que el script debe ser ejecutado con el intérprete de comandos bash.
* La segunda línea define una variable llamada `directorio_actual`, que contiene la ruta al directorio actual.
* La tercera línea define una variable llamada `fichero_temporal`, que contiene la ruta a un fichero temporal que se utilizará para almacenar los nombres de los ficheros encontrados.
* La cuarta línea define una variable llamada `fichero_salida`, que contiene la ruta al fichero de salida en el que se escribirán los resultados.
* La quinta línea utiliza el comando `find` para buscar todos los ficheros con extensión `.txt` en el directorio actual y en los subdirectorios. Los resultados de la búsqueda se escriben en el fichero temporal.
* La sexta línea utiliza el comando `while` para leer el fichero temporal línea por línea.
* La séptima línea obtiene el nombre del fichero actual de la línea actual del fichero temporal.
* La octava línea comprueba si el fichero actual es un enlace simbólico. Si lo es, la novena línea obtiene el destino del enlace simbólico.
* La décima línea comprueba si el destino del enlace simbólico existe. Si existe, la undécima línea escribe el nombre del fichero actual y el destino del enlace simbólico en el fichero de salida. Si no existe, la duodécima línea escribe el nombre del fichero actual y un mensaje de error en el fichero de salida.
* La decimotercera línea comprueba si el fichero actual no es un enlace simbólico. Si no lo es, la decimocuarta línea escribe el nombre del fichero actual en el fichero de salida.
* La decimoquinta línea elimina el fichero temporal.
* La decimosexta línea muestra el contenido del fichero de salida.