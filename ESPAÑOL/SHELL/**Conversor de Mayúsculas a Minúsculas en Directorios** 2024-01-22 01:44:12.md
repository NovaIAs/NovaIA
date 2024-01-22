```SHELL
#!/bin/bash

# VARIABLES
DIRECTORIO_ACTUAL=$(pwd)
DIRECTORIO_TEMPORAL=$(mktemp -d)
FICHERO_TEMPORAL=$(mktemp)
MENSAJE_AYUDA="Uso: ./script.sh [-h] [-d directorio] [-t tiempo]

Opciones:
  -h         Mostrar esta ayuda y salir.
  -d directorio  Directorio a procesar.
  -t tiempo     Tiempo en segundos para procesar el directorio.

Ejemplos:
  ./script.sh
  ./script.sh -d /tmp
  ./script.sh -t 10"

# FUNCIONES
mostrar_ayuda() {
  echo "$MENSAJE_AYUDA"
}

procesar_directorio() {
  local directorio=$1
  local tiempo=$2

  cd "$directorio" || exit 1

  # Encontrar todos los archivos con extensión .txt
  local ficheros=$(find . -name "*.txt")

  # Procesar cada uno de los archivos encontrados
  for fichero in $ficheros; do
    # Comprobar si el tiempo ha expirado
    if [[ $tiempo -gt 0 ]]; then
      ((tiempo--))
      if [[ $tiempo -eq 0 ]]; then
        echo "Tiempo expirado."
        break
      fi
    fi

    # Leer el contenido del archivo
    local contenido=$(cat "$fichero")

    # Convertir el contenido a minúsculas
    local contenido_minusculas=$(echo "$contenido" | tr '[:upper:]' '[:lower:]')

    # Escribir el contenido convertido en minúsculas en el archivo
    echo "$contenido_minusculas" > "$fichero"
  done

  cd "$DIRECTORIO_ACTUAL" || exit 1
}

# PROCESAMIENTO DE ARGUMENTOS
while getopts ":hd:t:" opcion; do
  case $opcion in
    h)
      mostrar_ayuda
      exit 0
      ;;
    d)
      DIRECTORIO_A_PROCESAR=$OPTARG
      ;;
    t)
      TIEMPO_A_PROCESAR=$OPTARG
      ;;
    \?)
      echo "Opción inválida -$OPTARG" >&2
      mostrar_ayuda
      exit 1
      ;;
  esac
done

# EJECUCIÓN DEL SCRIPT
if [[ -z $DIRECTORIO_A_PROCESAR ]]; then
  DIRECTORIO_A_PROCESAR=$DIRECTORIO_ACTUAL
fi

if [[ -z $TIEMPO_A_PROCESAR ]]; then
  TIEMPO_A_PROCESAR=0
fi

procesar_directorio "$DIRECTORIO_A_PROCESAR" "$TIEMPO_A_PROCESAR"

# LIMPIEZA
rm -rf "$DIRECTORIO_TEMPORAL"
rm "$FICHERO_TEMPORAL"
```

Explicación del código:

* El script comienza definiendo las variables `DIRECTORIO_ACTUAL`, `DIRECTORIO_TEMPORAL` y `FICHERO_TEMPORAL`. `DIRECTORIO_ACTUAL` es el directorio actual, `DIRECTORIO_TEMPORAL` es un directorio temporal creado con la función `mktemp -d` y `FICHERO_TEMPORAL` es un fichero temporal creado con la función `mktemp`.
* A continuación, se definen las funciones `mostrar_ayuda()` y `procesar_directorio()`. La función `mostrar_ayuda()` muestra el mensaje de ayuda y sale del script. La función `procesar_directorio()` procesa un directorio dado, convirtiendo a minúsculas el contenido de todos los archivos de texto que contiene.
* El script procesa los argumentos pasados por línea de comandos utilizando la función `getopts`. Los argumentos válidos son: `-h` para mostrar la ayuda, `-d` para especificar el directorio a procesar y `-t` para especificar el tiempo en segundos para procesar el directorio.
* Si no se especifica el directorio a procesar, se utiliza el directorio actual. Si no se especifica el tiempo a procesar, se utiliza el valor 0, lo que significa que se procesará el directorio hasta que se completen todos los archivos.
* El script llama a la función `procesar_directorio()` con los argumentos especificados por el usuario.
* Finalmente, el script limpia los ficheros temporales creados.