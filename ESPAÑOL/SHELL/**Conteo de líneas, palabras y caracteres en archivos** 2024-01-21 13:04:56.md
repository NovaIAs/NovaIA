```shell
#!/bin/bash

# Colores para el texto
ROJO='\033[0;31m'
VERDE='\033[0;32m'
AMARILLO='\033[0;33m'
AZUL='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
BLANCO='\033[0;37m'

# Opciones del script
while getopts ":hVr:" opt; do
  case $opt in
    h)
      echo "Uso: ./script.sh [-h] [-V] [-r <ruta_directorio>]"
      echo "Opción h: Mostrar esta ayuda."
      echo "Opción V: Mostrar la versión del script."
      echo "Opción r: Ruta al directorio donde se realizará el análisis."
      exit 0
      ;;
    V)
      echo "Versión 1.0"
      exit 0
      ;;
    r)
      RUTA_DIRECTORIO=$OPTARG
      ;;
    \?)
      echo "Opción inválida: -$OPTARG"
      exit 1
      ;;
  esac
done

# Comprobar si se ha proporcionado una ruta de directorio
if [ -z "$RUTA_DIRECTORIO" ]; then
  echo "No se ha proporcionado una ruta de directorio."
  echo "Uso: ./script.sh [-h] [-V] [-r <ruta_directorio>]"
  exit 1
fi

# Comprobar si el directorio existe
if [ ! -d "$RUTA_DIRECTORIO" ]; then
  echo "El directorio $RUTA_DIRECTORIO no existe."
  exit 1
fi

# Función para contar las líneas de código en un archivo
contar_lineas() {
  local ARCHIVO=$1
  local NUM_LINEAS=$(wc -l < "$ARCHIVO")
  echo $NUM_LINEAS
}

# Función para contar las palabras en un archivo
contar_palabras() {
  local ARCHIVO=$1
  local NUM_PALABRAS=$(wc -w < "$ARCHIVO")
  echo $NUM_PALABRAS
}

# Función para contar los caracteres en un archivo
contar_caracteres() {
  local ARCHIVO=$1
  local NUM_CARACTERES=$(wc -c < "$ARCHIVO")
  echo $NUM_CARACTERES
}

# Variables para almacenar los totales
TOTAL_LINEAS=0
TOTAL_PALABRAS=0
TOTAL_CARACTERES=0

# Recorrer todos los archivos del directorio
for ARCHIVO in "$RUTA_DIRECTORIO"/*; do
  # Comprobar si el archivo es regular
  if [ -f "$ARCHIVO" ]; then
    # Obtener el nombre del archivo
    NOMBRE_ARCHIVO=$(basename "$ARCHIVO")

    # Obtener el número de líneas, palabras y caracteres del archivo
    NUM_LINEAS=$(contar_lineas "$ARCHIVO")
    NUM_PALABRAS=$(contar_palabras "$ARCHIVO")
    NUM_CARACTERES=$(contar_caracteres "$ARCHIVO")

    # Sumar los totales
    TOTAL_LINEAS=$((TOTAL_LINEAS + NUM_LINEAS))
    TOTAL_PALABRAS=$((TOTAL_PALABRAS + NUM_PALABRAS))
    TOTAL_CARACTERES=$((TOTAL_CARACTERES + NUM_CARACTERES))

    # Mostrar la información del archivo
    echo "$NOMBRE_ARCHIVO:"
    echo "- Líneas: $NUM_LINEAS"
    echo "- Palabras: $NUM_PALABRAS"
    echo "- Caracteres: $NUM_CARACTERES"
  fi
done

# Mostrar los totales
echo "Totales:"
echo "- Líneas: $TOTAL_LINEAS"
echo "- Palabras: $TOTAL_PALABRAS"
echo "- Caracteres: $TOTAL_CARACTERES"
```

Este script recorre todos los archivos de un directorio y muestra el número de líneas, palabras y caracteres de cada archivo. También muestra los totales de todo el directorio.

El script utiliza las variables siguientes:

* `RUTA_DIRECTORIO`: la ruta del directorio que se va a analizar.
* `TOTAL_LINEAS`, `TOTAL_PALABRAS` y `TOTAL_CARACTERES`: variables para almacenar los totales de líneas, palabras y caracteres de todos los archivos del directorio.

El script utiliza las funciones siguientes:

* `contar_lineas()`: cuenta el número de líneas en un archivo.
* `contar_palabras()`: cuenta el número de palabras en un archivo.
* `contar_caracteres()`: cuenta el número de caracteres en un archivo.

El script utiliza las opciones de línea de comandos siguientes:

* `-h`: muestra la ayuda.
* `-V`: muestra la versión.
* `-r`: especifica la ruta del directorio que se va a analizar.

El script utiliza los colores siguientes:

* `ROJO`: rojo
* `VERDE`: verde
* `AMARILLO`: amarillo
* `AZUL`: azul
* `MAGENTA`: magenta
* `CYAN`: cian
* `BLANCO`: blanco