```shell
#!/bin/bash

# Variables

# Lista de directorios a procesar
directorios="/home/usuario/directorio1 /home/usuario/directorio2 /home/usuario/directorio3"

# Lista de extensiones de archivos a buscar
extensiones="txt jpg png pdf mp4"

# Función para buscar archivos en un directorio

buscar_archivos() {
  # Directorio a procesar
  directorio=$1

  # Extensiones de archivos a buscar
  extensiones=$2

  # Lista de archivos encontrados
  archivos=()

  # Recorrer el directorio y buscar archivos con las extensiones especificadas
  for extension in $extensiones; do
    archivos+=($(find $directorio -name "*.$extension"))
  done

  # Devolver la lista de archivos encontrados
  echo "${archivos[@]}"
}

# Función para procesar un archivo

procesar_archivo() {
  # Archivo a procesar
  archivo=$1

  # Obtener el tamaño del archivo en bytes
  tamanio=$(wc -c < $archivo)

  # Obtener la fecha de última modificación del archivo
  fecha=$(stat -c %y $archivo)

  # Imprimir el nombre del archivo, el tamaño y la fecha de última modificación
  echo "$archivo ($tamanio bytes, $fecha)"
}

# Procesar los directorios y archivos

# Recorrer la lista de directorios
for directorio in $directorios; do

  # Buscar archivos en el directorio
  archivos=$(buscar_archivos $directorio "$extensiones")

  # Procesar los archivos encontrados
  for archivo in $archivos; do

    # Procesar el archivo
    procesar_archivo $archivo

  done

done
```

Explicación del código:

* La primera línea indica la ruta del intérprete de comandos que se utilizará para ejecutar el script. En este caso, es "/bin/bash".

* La siguiente sección define las variables que se utilizarán en el script.

* La función `buscar_archivos()` se utiliza para buscar archivos en un directorio con las extensiones especificadas. La función recibe dos parámetros: el directorio a procesar y la lista de extensiones de archivos a buscar. La función devuelve una lista de los archivos encontrados.

* La función `procesar_archivo()` se utiliza para procesar un archivo. La función recibe un parámetro: el archivo a procesar. La función imprime el nombre del archivo, el tamaño del archivo en bytes y la fecha de última modificación del archivo.

* El código principal del script recorre la lista de directorios, busca archivos en cada directorio y procesa los archivos encontrados.